package com.w47s0n.chunkala.internal

/** A half-open range of char indices into the source. */
private[chunkala] final case class Span(start: Int, end: Int)

/** The smallest piece of source the merge pass will keep whole, together with how significant the
  * boundary in front of it is.
  *
  * @param boundaryLevel
  *   the depth at which this atom was cut off from its predecessor — lower is a stronger boundary.
  *   `0` is the edge of the document, `1` the whole source, and every level of descent adds one. A
  *   blank line therefore outranks a comma because the separator ladder reaches it first, and a
  *   method boundary outranks a statement within it because the syntax tree does.
  *
  * Only differences between levels are ever charged for, but the document edge is pinned at `0`, so
  * the depths cannot be shifted freely.
  *
  * @param internalBreak
  *   the hardest line break inside this atom
  * @param boundaryBreak
  *   the hardest line break in the gap before this atom
  */
private[chunkala] final case class Atom(
    start: Int,
    end: Int,
    boundaryLevel: Int,
    internalBreak: LineBreak,
    boundaryBreak: LineBreak
)

private[chunkala] object Atom {

  /** The boundary level of the document's own edges. */
  val DocumentLevel: Int = 0
}
