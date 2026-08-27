package com.w47s0n.chunkala.internal

import com.w47s0n.chunkala.{ParseError, ScalaDialect}
// Wildcard rather than named imports: the `dialect(input).parse[Source]` form below needs the
// extension methods scalameta exposes from its package root.
import scala.meta.*

/** Adapts a scalameta parse tree to [[SyntaxNode]]. */
private[chunkala] object ScalametaTree {

  /** A successful parse and which dialect produced it. */
  final case class Parse(root: SyntaxNode, dialect: ScalaDialect)

  /** Parse with each dialect in turn, first success wins.
    *
    * scalameta has no error recovery: a source either parses whole or not at all. The error
    * reported on failure is the last dialect's, which is the most useful one when the list is
    * ordered newest-first — a Scala 2 file rejected by both dialects fails more informatively under
    * the 2.13 parser.
    */
  def parse(text: String, dialects: Seq[ScalaDialect]): Either[ParseError, Parse] = {
    @annotation.tailrec
    def firstThatParses(
        remaining: List[ScalaDialect],
        lastError: Option[ParseError]
    ): Either[ParseError, Parse] =
      remaining match {
        case Nil =>
          Left(lastError.getOrElse(ParseError("no dialect to parse with", 1, 1)))
        case dialect :: rest =>
          attempt(text, dialect) match {
            case Right(parsed) => Right(Parse(parsed, dialect))
            case Left(error) => firstThatParses(rest, Some(error))
          }
      }

    firstThatParses(dialects.toList, None)
  }

  private def attempt(text: String, dialect: ScalaDialect): Either[ParseError, SyntaxNode] =
    dialect.underlying(Input.String(text)).parse[Source] match {
      case Parsed.Success(tree) => Right(MetaNode(tree))
      case Parsed.Error(pos, message, _) =>
        // scalameta counts lines and columns from zero; the rest of this API counts from one, as
        // editors do.
        Left(ParseError(message, pos.startLine + 1, pos.startColumn + 1))
    }

  /** A scalameta `Tree` seen as a [[SyntaxNode]]. */
  private final class MetaNode(tree: Tree) extends SyntaxNode {
    def start: Int = tree.pos.start

    def end: Int = tree.pos.end

    def children: Vector[SyntaxNode] = MetaNode.walkable(tree)
  }

  private object MetaNode {

    private final case class GroupNode(
        start: Int,
        end: Int,
        children: Vector[SyntaxNode]
    ) extends SyntaxNode

    /** The children the atomizer can walk, repaired to meet its expectations.
      *
      * scalameta makes none of the guarantees [[SyntaxNode]] requires. Synthetic nodes carry no
      * position at all; children arrive in the order the tree declares them rather than the order
      * they appear in source (a `Defn.Def` lists modifiers, name, params and body, which is not
      * always textual order); and some nodes nest inside a sibling's range.
      *
      * Empty nodes are dropped, the rest are sorted, and any that would overlap the previous one is
      * dropped too. Dropping loses no text — whatever a discarded node covered becomes a gap, and
      * the separator ladder chunks gaps — it only forgoes that node's structure.
      */
    def walkable(tree: Tree): Vector[SyntaxNode] =
      tree match {
        case definition: Defn.Class => templateOwner(definition, definition.templ)
        case _ => repaired(tree.children)
      }

    /** Keep an owner and its parent initializers under one structural boundary.
      *
      * scalameta puts a class's constructor and template beside each other, and the template starts
      * at `extends`. Treating those siblings as equally strong boundaries allows the merger to put
      * `extends Parent` in a chunk without the child declaration. The synthetic group makes cuts
      * inside the complete header weaker than the boundary between the header and body, while
      * retaining the original children so an oversized header can still be split.
      */
    private def templateOwner(owner: Tree, template: Template): Vector[SyntaxNode] = {
      val parents = template.inits.filter(node => node.pos.start < node.pos.end)
      parents.lastOption match {
        case None => repaired(owner.children)
        case Some(lastParent) =>
          val headerEnd = lastParent.pos.end
          val ownerParts =
            owner.children.filterNot(child => child.pos.start == template.pos.start &&
              child.pos.end == template.pos.end)
          val headerParts = ownerParts.filter(_.pos.end <= headerEnd) ++ parents
          val header =
            GroupNode(owner.pos.start, headerEnd, repaired(headerParts))
          val bodyParts =
            repaired(template.children.filter(_.pos.start >= headerEnd))
          header +: bodyParts
      }
    }

    private def repaired(children: Iterable[Tree]): Vector[SyntaxNode] = {
      val ordered = children.iterator
        .filter(child => child.pos.start < child.pos.end)
        .toVector
        .sortBy(_.pos.start)

      ordered
        .foldLeft((Vector.empty[SyntaxNode], Int.MinValue)) { case ((kept, reach), child) =>
          if (child.pos.start >= reach) (kept :+ MetaNode(child), child.pos.end)
          else (kept, reach)
        }
        ._1
    }
  }
}
