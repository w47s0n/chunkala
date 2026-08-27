package com.w47s0n.chunkala.internal

import com.w47s0n.chunkala.{Boundaries, ChunkerConfig, Chunking}

/** Runs the three passes for one configuration.
  *
  * Split from [[com.w47s0n.chunkala.Chunker]] so the public class stays a thin, documented surface
  * and the pass wiring has somewhere to live that is not part of the published API.
  */
private[chunkala] final class Engine(config: ChunkerConfig) {

  private val limits = Limits(config.targetSize, config.minSize, config.effectiveOverlap)

  private val ladder = config.separators.patterns

  def run(text: String, tree: Option[SyntaxNode], boundaries: Boundaries): Chunking =
    if (text.isEmpty) Chunking(Vector.empty, boundaries)
    else {
      val bytes = ByteIndex(text)
      val atoms = Atomizer.atomize(text, bytes, ladder, tree, limits.minAtomBytes)
      val spans = Merger.merge(atoms, bytes, limits)
      Chunking(Locator.locate(text, bytes, spans), boundaries)
    }
}
