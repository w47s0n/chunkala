package com.w47s0n.chunkala

import java.util.regex.{Pattern, PatternSyntaxException}

/** An ordered ladder of separator patterns, most significant boundary first.
  *
  * Chunking descends this ladder: it cuts on the first rung, and only if a piece is still too large
  * does it try the next. So the order encodes priority — a blank line is a better place to break
  * than a comma, and a comma is better than an arbitrary space.
  *
  * Patterns are Java regexes compiled with Unicode-aware character classes, so `\s` covers the full
  * Unicode `White_Space` property rather than ASCII alone.
  */
final class Separators private (private[chunkala] val patterns: Vector[Pattern]) {

  /** How many rungs the ladder has. */
  def size: Int = patterns.size

  /** The source of each rung, in order. */
  def sources: Vector[String] = patterns.map(_.pattern)

  override def toString: String = sources.mkString("Separators(", ", ", ")")

  override def equals(that: Any): Boolean = that match {
    case other: Separators => sources == other.sources
    case _ => false
  }

  override def hashCode: Int = sources.hashCode
}

object Separators {

  /** `.` must mean "any character but `\n`" and `\s` must be Unicode-aware, to match the semantics
    * the default ladder was designed against.
    */
  private val Flags = Pattern.UNICODE_CHARACTER_CLASS | Pattern.UNIX_LINES

  /** Prose and code boundaries in descending order of significance: blank line, line, sentence,
    * clause, list item, any whitespace.
    *
    * The CJK alternatives are not decoration — full-width punctuation is how those scripts mark the
    * same boundaries, and without them a CJK document has no usable break below "line".
    */
  private val DefaultSources: Vector[String] = Vector(
    "\\n\\n+",
    "\\n",
    "[\\.\\?!]\\s+|。|？|！",
    "[;:\\-—]\\s+|；|：|—+",
    ",\\s+|，",
    "\\s+"
  )

  /** The ladder used unless a caller supplies another. */
  val Default: Separators = new Separators(DefaultSources.map(Pattern.compile(_, Flags)))

  /** Build a ladder, reporting the first pattern that does not compile.
    *
    * An empty ladder is allowed and means "never cut on separators": with a parse tree it still
    * chunks on syntax, and without one every source becomes a single chunk.
    */
  def from(patterns: Seq[String]): Either[InvalidSeparator, Separators] = {
    val compiled = Vector.newBuilder[Pattern]
    val failure = patterns.iterator
      .map { source =>
        try {
          compiled += Pattern.compile(source, Flags)
          None
        } catch {
          case e: PatternSyntaxException => Some(InvalidSeparator(source, e.getDescription))
        }
      }
      .collectFirst { case Some(error) => error }
    failure.toLeft(new Separators(compiled.result()))
  }

  /** Build a ladder, throwing if a pattern does not compile.
    *
    * For literals written at the call site, where a bad pattern is a programming error rather than
    * something to handle. Prefer [[from]] for anything a user supplies.
    */
  def of(patterns: String*): Separators =
    from(patterns).fold(error => throw IllegalArgumentException(error.toString), identity)
}

/** A separator pattern that is not a valid regex. */
final case class InvalidSeparator(pattern: String, message: String) {
  override def toString: String = s"invalid separator `$pattern`: $message"
}
