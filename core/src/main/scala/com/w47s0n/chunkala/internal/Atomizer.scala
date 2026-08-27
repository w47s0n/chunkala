package com.w47s0n.chunkala.internal

import java.util.regex.{Matcher, Pattern}

/** Phase one: cut the source into [[Atom]]s.
  *
  * Descends the syntax tree where there is one and the separator ladder where there is not,
  * stopping as soon as a piece is small enough that the merge pass would never want to split it
  * further. What comes out is a flat sequence of atoms in source order, each tagged with how
  * significant the boundary in front of it is.
  *
  * The walk has one rule, in [[Atomizer.atomize]]'s `descend`: cut a piece into sub-pieces, and
  * recurse into each. What differs between a ladder rung and a syntax node is only how their
  * sub-pieces are enumerated, which is [[Cut.of]]'s job. Keeping those apart is what stops the
  * boundary bookkeeping from being written twice and drifting.
  */
private[chunkala] object Atomizer {

  /** What the walk is currently cutting. */
  private enum Piece {

    /** A rung of the separator ladder. */
    case Rung(index: Int)

    /** A node of the parse tree. */
    case Node(node: SyntaxNode)
  }

  /** One sub-piece of a larger piece: a range, and how to cut it further. */
  private final case class Cut(span: Span, piece: Piece)

  /** Atoms collected so far, and the strongest boundary crossed since the last.
    *
    * `pending` is not simply the depth of the current cut. A piece that trims away to nothing emits
    * no atom, so the boundary it would have carried has to survive until a piece does emit —
    * otherwise a paragraph break followed by blank lines would look no stronger than the space
    * after a comma.
    */
  private final case class Acc(atoms: Vector[Atom], pending: Int) {
    def crossing(level: Int): Acc = if (level < pending) copy(pending = level) else this

    def emitted(atom: Atom, depth: Int): Acc = Acc(atoms :+ atom, depth)

    def lastEnd: Int = if (atoms.isEmpty) 0 else atoms.last.end
  }

  /** State threaded across the sub-pieces of one piece.
    *
    * `boundary` starts as the parent's own left boundary — the first sub-piece begins exactly where
    * its parent does — and drops to the cutting depth for every sub-piece after it.
    */
  private final case class Walk(acc: Acc, boundary: Int) {
    def after(depth: Int, next: Acc): Walk = Walk(next, depth)
  }

  /** Cut `text` into atoms.
    *
    * @param root
    *   the parse tree to follow, or `None` to use the ladder from the top
    * @param minAtomBytes
    *   the size at or below which a piece is kept whole
    */
  def atomize(
      text: String,
      bytes: ByteIndex,
      ladder: Vector[Pattern],
      root: Option[SyntaxNode],
      minAtomBytes: Int
  ): Vector[Atom] = {

    /** Trim a piece, align it to its line, and record it — unless it is blank.
      *
      * `depth` becomes the floor for the next atom's boundary: nothing after this point can claim a
      * stronger boundary than the level we cut at.
      */
    def emit(span: Span, depth: Int, acc: Acc): Acc = {
      val end = Text.trimmedEnd(text, span.start, span.end)
      if (end == span.start) acc // blank: `pending` carries on to the next piece
      else {
        val start = Text.trimmedStart(text, span.start, end)
        val previousEnd = acc.lastEnd
        val boundaryBreak = Text.lineBreakOf(text, previousEnd, start)
        // When a line break separates this atom from the last, begin it at the head of its line so
        // indentation stays attached to the code it indents.
        val alignedStart =
          if (boundaryBreak == LineBreak.Inline) start
          else Text.startOfIndent(text, start, previousEnd)
        acc.emitted(
          Atom(
            start = alignedStart,
            end = end,
            boundaryLevel = acc.pending,
            internalBreak = Text.lineBreakOf(text, start, end),
            boundaryBreak = boundaryBreak
          ),
          depth
        )
      }
    }

    /** Cut `span` down to atoms.
      *
      * `depth` is the whole of the boundary arithmetic: a piece that stops here becomes an atom at
      * `depth`, and anything it is cut into starts one deeper.
      */
    def descend(span: Span, piece: Piece, depth: Int, boundary: Int, incoming: Acc): Acc = {
      val acc = incoming.crossing(boundary)
      val exhausted = piece match {
        case Piece.Rung(index) => index >= ladder.length
        case Piece.Node(_) => false
      }

      if (exhausted || bytes.lengthOf(span.start, span.end) <= minAtomBytes)
        emit(span, depth, acc)
      else
        Cut
          .of(piece, span, text, ladder)
          .foldLeft(Walk(acc, boundary)) { (walk, cut) =>
            walk.after(depth + 1, descend(cut.span, cut.piece, depth + 1, walk.boundary, walk.acc))
          }
          .acc
    }

    val whole = Span(0, text.length)
    val start = root.fold(Piece.Rung(0))(Piece.Node.apply)
    val walked =
      descend(whole, start, depth = 1, Atom.DocumentLevel, Acc(Vector.empty, Atom.DocumentLevel))

    // A sentinel atom marks the end of the document so the merge pass can ask about the boundary
    // *after* the final real atom without a special case.
    walked.atoms :+ Atom(
      start = text.length,
      end = text.length,
      boundaryLevel = Atom.DocumentLevel,
      internalBreak = LineBreak.Inline,
      boundaryBreak = LineBreak.Blank
    )
  }

  private object Cut {

    /** The sub-pieces of `piece`, in source order and covering `span` completely.
      *
      * Gaps matter as much as children. Text between two syntax nodes belongs to neither — that is
      * where punctuation, comments and blank lines live — so it is handed to the ladder rather than
      * dropped, and a leaf node is simply the case where the gap is the whole range.
      */
    def of(piece: Piece, span: Span, text: String, ladder: Vector[Pattern]): Iterator[Cut] =
      piece match {
        case Piece.Rung(index) =>
          splitOn(text, span, ladder(index)).map(Cut(_, Piece.Rung(index + 1)))

        case Piece.Node(node) =>
          val (cuts, cursor) = node.children
            .foldLeft((Vector.empty[Cut], span.start)) { case ((acc, cursor), child) =>
              val gap = Option.when(cursor < child.start)(ladderCut(Span(cursor, child.start)))
              (acc ++ gap :+ Cut(Span(child.start, child.end), Piece.Node(child)), child.end)
            }
          val trailing = Option.when(cursor < span.end)(ladderCut(Span(cursor, span.end)))
          (cuts ++ trailing).iterator
      }

    private def ladderCut(span: Span): Cut = Cut(span, Piece.Rung(0))

    /** Where the scan through a range has got to. */
    private enum Scan {
      case Searching(cursor: Int)
      case Finished
    }

    /** The spans of `span` lying between successive matches of `sep`.
      *
      * Lazy, and empty spans are yielded rather than skipped so that a run of adjacent separators
      * still registers as a single strong boundary rather than several weak ones.
      *
      * The matcher is a mutable engine, but it is driven strictly forward and never escapes, and
      * the scan's own position lives in the unfold's state rather than in a captured var.
      */
    private def splitOn(text: String, span: Span, sep: Pattern): Iterator[Span] = {
      val matcher: Matcher = sep.matcher(text).region(span.start, span.end)
      Iterator.unfold[Span, Scan](Scan.Searching(span.start)) {
        case Scan.Finished => None
        case Scan.Searching(cursor) =>
          if (matcher.find()) Some((Span(cursor, matcher.start), Scan.Searching(matcher.end)))
          else Option.when(cursor < span.end)((Span(cursor, span.end), Scan.Finished))
      }
    }
  }
}
