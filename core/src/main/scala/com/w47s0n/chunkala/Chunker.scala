package com.w47s0n.chunkala

import com.w47s0n.chunkala.internal.{Engine, ScalametaTree}

/** Splits source text into chunks that respect its structure.
  *
  * Chunking runs in two passes. The first cuts the source down to the smallest pieces worth keeping
  * whole — descending the syntax tree where there is one, and the separator ladder where there is
  * not — recording how significant the boundary before each piece is. The second groups those
  * pieces back into chunks, choosing the grouping that minimises a cost: exceeding
  * [[ChunkerConfig.targetSize]] is forbidden, falling below [[ChunkerConfig.minSize]] is expensive,
  * and breaking at a weak boundary is expensive in proportion to the stronger boundaries the chunk
  * swallowed.
  *
  * That second pass is what separates this from a fixed-window splitter: it will accept a short
  * chunk to end at a method boundary rather than a full one that ends mid-expression.
  *
  * Instances are immutable and safe to share between threads.
  *
  * @example
  *   {{{
  *   val chunker = Chunker(targetSize = 2000)
  *   val result  = chunker.chunkScala(source)
  *   result.chunks.foreach(c => println(s"\${c.start.line}: \${c.text.take(40)}"))
  *   }}}
  */
final class Chunker(val config: ChunkerConfig) {

  private val engine = Engine(config)

  /** Chunk text with no syntax awareness, using the separator ladder alone.
    *
    * The right choice for prose, and for source in a language this library cannot parse.
    */
  def chunkText(source: String): Chunking =
    engine.run(source, tree = None, boundaries = Boundaries.Separators)

  /** Chunk Scala source, following its syntax tree.
    *
    * Dialects are tried in order and the first that parses wins. If none does, the source is still
    * chunked — by separators — and the result carries the [[ParseError]] so the caller can tell
    * that happened.
    */
  def chunkScala(source: String, dialects: Seq[ScalaDialect] = ScalaDialect.Auto): Chunking =
    ScalametaTree.parse(source, dialects) match {
      case Right(parsed) =>
        engine.run(source, Some(parsed.root), Boundaries.Syntax(parsed.dialect))
      case Left(error) =>
        engine.run(source, tree = None, boundaries = Boundaries.ParseFailed(error))
    }

  /** Chunk a file, choosing how by its name.
    *
    * Scala extensions (`.scala`, `.sc`, `.sbt`) are parsed; anything else is treated as text. Only
    * the extension is read — the file itself is not touched, `source` is the content.
    */
  def chunkFile(filename: String, source: String): Chunking =
    ScalaDialect.forFile(filename) match {
      case Some(dialects) => chunkScala(source, dialects)
      case None => chunkText(source)
    }

  override def toString: String = s"Chunker($config)"
}

object Chunker {

  /** A chunker with the given configuration. */
  def apply(config: ChunkerConfig): Chunker = new Chunker(config)

  /** A chunker with the given size ceiling and otherwise default settings. */
  def apply(targetSize: Int): Chunker = new Chunker(ChunkerConfig(targetSize))

  /** A chunker with [[ChunkerConfig.Default]]. */
  val Default: Chunker = new Chunker(ChunkerConfig.Default)
}
