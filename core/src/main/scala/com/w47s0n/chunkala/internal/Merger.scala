package com.w47s0n.chunkala.internal

import scala.collection.mutable

/** Phase two: group atoms into the cheapest set of chunks the limits allow.
  *
  * A chunk running from atom `i` to atom `j` costs:
  *
  *   - a large penalty if it is shorter than the minimum;
  *   - a penalty for each boundary inside it stronger than the ones it is cut at, falling off as
  *     `512/1 + 512/2 + …` so the first level of mismatch dominates and deep nesting does not swamp
  *     everything else;
  *   - a smaller penalty of the same shape for line breaks;
  *   - a penalty for overlap it fails to provide with its predecessor.
  *
  * Anything longer than the ceiling is not a candidate at all, unless a single atom already exceeds
  * it — an unsplittable token has to become a chunk of its own however long it is.
  *
  * Minimising that over all groupings is a shortest-path problem, solved here by a dynamic program
  * over atom index. The implementation is deliberately imperative: it is a textbook DP with an
  * array-backed memo and a priority queue, and expressing the recurrence as a fold would allocate
  * per candidate and hide the very structure that makes it linear-ish. Nothing mutable escapes, so
  * the function is pure from the outside.
  */
private[chunkala] object Merger {

  private val SyntaxLevelGapCost = 512L
  private val MissingOverlapCost = 512L
  private val LineBreakGapCost = 64L
  private val UndersizeCost = 1048576L

  /** Group `atoms` (including the end sentinel) into chunk spans. */
  def merge(atoms: Vector[Atom], bytes: ByteIndex, limits: Limits): Vector[Span] = {
    val n = atoms.length

    // plan(k) is the best way to cover the source through the end of atom k-1; plan(0) is the empty
    // prefix.
    val cost = new Array[Long](n)
    val firstAtom = new Array[Int](n)
    val predecessor = new Array[Int](n)
    val deficit = new Array[Long](n)
    deficit(0) = overlapDeficit(0, bytes, limits)

    val levelGap = levelGapCosts()

    // Cheapest reachable predecessor first; ties go to the later atom so the choice is
    // deterministic.
    given Ordering[(Long, Int)] = Ordering.fromLessThan { (a, b) =>
      if (a._1 != b._1) a._1 > b._1 else a._2 < b._2
    }
    val reachable = mutable.PriorityQueue.empty[(Long, Int)]

    var last = 0
    while (last < n - 1) {
      val endLevel = atoms(last + 1).boundaryLevel
      val endBreak = atoms(last + 1).boundaryBreak

      var best = Long.MaxValue
      var bestFirst = 0
      var bestPredecessor = 0

      // Widen the chunk leftwards one atom at a time, tracking the strongest boundary swallowed so
      // far.
      var first = last
      var innerLevel = Int.MaxValue
      var innerBreak = LineBreak.Inline
      var widening = true

      while (widening) {
        val head = atoms(first)
        val size = bytes.lengthOf(head.start, atoms(last).end)

        var candidate =
          levelGap(head.boundaryLevel, innerLevel) + levelGap(endLevel, innerLevel) +
            breakGap(head.boundaryBreak, innerBreak) + breakGap(endBreak, innerBreak)
        if (size < limits.minSize) candidate += UndersizeCost

        if (size > limits.targetSize) {
          // Over the ceiling, and widening only makes it worse. If nothing narrower fitted either,
          // this single atom becomes its own chunk.
          if (best == Long.MaxValue) {
            best = candidate + cost(first)
            bestFirst = first
            bestPredecessor = first
          }
          widening = false
        } else {
          val chosen =
            if (limits.overlap == 0) first
            else {
              // Discard plans whose end has fallen outside this chunk's overlap window. `first`
              // only moves left, so a plan that drops out can never come back — which makes the top
              // of the queue, once the stale entries are gone, exactly the cheapest reachable plan.
              while (
                reachable.nonEmpty &&
                bytes.lengthOf(head.start, atoms(reachable.head._2).end) > limits.overlap
              ) reachable.dequeue(): Unit
              reachable.enqueue((cost(first) + deficit(first), first))
              reachable.head._2
            }

          val overlapPenalty =
            if (limits.overlap == 0) MissingOverlapCost / 2
            else
              MissingOverlapCost +
                (deficit(chosen) - overlapDeficit(head.start, bytes, limits)).min(0L)
          candidate += cost(chosen) + overlapPenalty

          if (candidate < best) {
            best = candidate
            bestFirst = first
            bestPredecessor = chosen
          }

          if (first == 0) widening = false
          else {
            first -= 1
            innerLevel = innerLevel min head.boundaryLevel
            innerBreak =
              if (head.internalBreak.ordinal > innerBreak.ordinal) head.internalBreak
              else innerBreak
          }
        }
      }

      cost(last + 1) = best
      firstAtom(last + 1) = bestFirst
      predecessor(last + 1) = bestPredecessor
      deficit(last + 1) = overlapDeficit(atoms(last).end, bytes, limits)
      reachable.clear()
      last += 1
    }

    // Walk the winning plan back to the start of the source.
    Vector
      .unfold(n - 1) { plan =>
        Option.when(plan > 0) {
          (Span(atoms(firstAtom(plan)).start, atoms(plan - 1).end), predecessor(plan))
        }
      }
      .reverse
  }

  /** How much overlap cost is still owed between `charIndex` and the end.
    *
    * Predecessors nearer the end have already paid more of it, which is what makes a plan that
    * leaves room for the next chunk's overlap win.
    */
  private def overlapDeficit(charIndex: Int, bytes: ByteIndex, limits: Limits): Long =
    if (limits.overlap == 0) 0L
    else (bytes.totalBytes - bytes.byteAt(charIndex)).toLong * MissingOverlapCost / limits.overlap

  /** `512/1 + 512/2 + … + 512/gap`, memoised as the gap grows.
    *
    * The first level of mismatch costs full price and each further one less, so a cut in the wrong
    * place is punished sharply without a deeply nested gap dominating every other consideration.
    */
  private def levelGapCosts(): (Int, Int) => Long = {
    val cache = mutable.ArrayBuffer(0L)
    (boundary, inner) =>
      if (boundary <= inner) 0L
      else {
        val gap = boundary - inner
        while (cache.length <= gap) cache += cache.last + SyntaxLevelGapCost / cache.length
        cache(gap)
      }
  }

  private def breakGap(boundary: LineBreak, inner: LineBreak): Long =
    (inner.ordinal - boundary.ordinal).max(0).toLong * LineBreakGapCost
}

/** The resolved, byte-denominated limits the merge pass works to. */
private[chunkala] final case class Limits(targetSize: Int, minSize: Int, overlap: Int) {

  /** The size at or below which a piece is never cut further.
    *
    * With overlap on, atoms have to be at least as fine as the overlap or no chunk boundary could
    * ever land inside the overlap window.
    */
  def minAtomBytes: Int = if (overlap > 0) overlap else minSize
}
