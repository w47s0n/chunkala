package com.w47s0n.chunkala.internal

/** The only thing chunking needs from a parse tree: a range, and children.
  *
  * Keeping the atomizer behind this interface means it holds no opinion about which parser produced
  * the tree, and lets the tree-walking rules be tested without parsing anything.
  *
  * Implementations must satisfy what the atomizer relies on:
  *   - children lie within the parent's range;
  *   - children are in source order and do not overlap each other;
  *   - every node is non-empty (`start < end`).
  *
  * Gaps between children are expected and fine — that is where punctuation and comments live, and
  * the separator ladder handles them.
  */
private[chunkala] trait SyntaxNode {
  def start: Int

  def end: Int

  /** Children worth descending into, in source order. Empty for a leaf.
    *
    * Computed on demand: chunking stops descending once a piece is small enough, so for large chunk
    * sizes most of the tree is never visited.
    */
  def children: Vector[SyntaxNode]
}
