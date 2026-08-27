package com.w47s0n.chunkala

/** A location in the source, in every unit a caller is likely to need.
  *
  * @param byte
  *   offset in UTF-8 bytes — the unit chunk sizes are expressed in
  * @param char
  *   offset in Unicode code points, so a surrogate pair counts once
  * @param line
  *   1-based line number
  * @param column
  *   1-based column, counted in code points
  */
final case class Position(byte: Int, char: Int, line: Int, column: Int)

/** A piece of the source together with where it came from.
  *
  * `text` is exactly the source between [[start]] and [[end]]. Chunks are in source order and never
  * split a code point. They do not necessarily tile the source: whitespace at a boundary is
  * dropped, and with overlap configured neighbouring chunks share text.
  */
final case class Chunk(text: String, start: Position, end: Position) {

  /** Size in UTF-8 bytes, the unit [[ChunkerConfig.targetSize]] is measured in.
    *
    * This is not `text.length`, which counts UTF-16 code units.
    */
  def byteLength: Int = end.byte - start.byte

  /** Size in Unicode code points. */
  def charLength: Int = end.char - start.char

  /** The 1-based lines this chunk touches, inclusive of both ends. */
  def lineSpan: Range = start.line to end.line
}

/** Why a chunk run has the boundaries it has.
  *
  * Worth inspecting: a Scala file that fails to parse still produces chunks, but they follow
  * separators rather than syntax, and that is usually worth surfacing rather than silently
  * accepting.
  */
enum Boundaries {

  /** The source parsed and chunks follow its syntax tree. */
  case Syntax(dialect: ScalaDialect)

  /** No parse was attempted — plain text, or a filename that is not Scala. */
  case Separators

  /** A parse was attempted and failed, so chunking fell back to separators.
    *
    * scalameta has no error recovery, so this is all or nothing: one bad construct anywhere costs
    * the syntax awareness of the whole file.
    */
  case ParseFailed(error: ParseError)
}

/** Why a parse did not succeed, as reported by the last dialect tried. */
final case class ParseError(message: String, line: Int, column: Int) {
  override def toString: String = s"$line:$column: $message"
}

/** The result of chunking one source: the chunks, and how they were chosen. */
final case class Chunking(chunks: Vector[Chunk], boundaries: Boundaries) {

  def size: Int = chunks.size

  def isEmpty: Boolean = chunks.isEmpty

  def nonEmpty: Boolean = chunks.nonEmpty

  /** Just the text of each chunk, for the common case of feeding an embedder. */
  def texts: Vector[String] = chunks.map(_.text)

  /** True when boundaries follow the parse tree rather than separators alone. */
  def isSyntaxAware: Boolean = boundaries match {
    case Boundaries.Syntax(_) => true
    case _ => false
  }

  /** The parse failure, if one is why these boundaries are separator-based. */
  def parseError: Option[ParseError] = boundaries match {
    case Boundaries.ParseFailed(error) => Some(error)
    case _ => None
  }
}
