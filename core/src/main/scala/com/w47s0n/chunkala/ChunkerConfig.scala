package com.w47s0n.chunkala

/** How a [[Chunker]] should size and place its chunks.
  *
  * All sizes are in UTF-8 bytes, not characters, because the consumers of chunks — embedding
  * models, context windows, storage limits — are themselves specified in bytes or tokens rather
  * than UTF-16 code units.
  *
  * @param targetSize
  *   the size no chunk should exceed. It is a ceiling rather than a goal: a chunk only exceeds it
  *   when a single indivisible piece of source is larger, in which case that piece becomes its own
  *   oversized chunk.
  * @param minSize
  *   the size below which a chunk is heavily penalised. Not a hard floor — producing a short chunk
  *   is preferred to breaking in a much worse place — but chunks fall short only when the
  *   alternatives are worse.
  * @param overlap
  *   how much text consecutive chunks may share. Zero means no overlap; the effective value is
  *   capped at `minSize`, since a larger overlap could not be honoured without producing chunks
  *   smaller than the minimum.
  * @param separators
  *   the ladder of fallback boundaries, used for text that has no parse tree and for the gaps
  *   between syntax nodes.
  */
final case class ChunkerConfig(
    targetSize: Int,
    minSize: Int,
    overlap: Int,
    separators: Separators
) {
  require(targetSize > 0, s"targetSize must be positive, got $targetSize")
  require(minSize >= 0, s"minSize must not be negative, got $minSize")
  require(minSize <= targetSize, s"minSize ($minSize) must not exceed targetSize ($targetSize)")
  require(overlap >= 0, s"overlap must not be negative, got $overlap")

  /** The overlap actually applied, capped at [[minSize]].
    *
    * Capping rather than rejecting: asking for more overlap than the minimum chunk size is a
    * reasonable thing to express ("overlap generously") and the cap is the most overlap that can be
    * delivered.
    */
  def effectiveOverlap: Int = overlap min minSize
}

object ChunkerConfig {

  /** A config with the given ceiling, a minimum of half that, and no overlap. */
  def apply(targetSize: Int): ChunkerConfig =
    ChunkerConfig(targetSize, targetSize / 2, 0, Separators.Default)

  /** A reasonable starting point: chunks of at most 1000 bytes, no overlap. */
  val Default: ChunkerConfig = ChunkerConfig(1000)
}
