package com.w47s0n.chunkala.internal

/** How hard a line break is.
  *
  * Ordering matters: chunking prefers to cut where the break is at least as hard as any break
  * inside the chunk, so a paragraph boundary is not left dangling in the middle of one.
  */
private[chunkala] enum LineBreak {
  case Inline, Newline, Blank
}

/** Character-level predicates and scans shared by the chunking passes.
  *
  * These are `while` loops rather than folds on purpose: they run once per atom on the hot path,
  * and a per-character allocation would cost more than the clarity is worth. Each is a pure
  * function over its arguments — the mutation never leaves the method.
  */
private[chunkala] object Text {

  /** The Unicode `White_Space` property.
    *
    * Not `Character.isWhitespace`, which is a different set: it excludes NBSP and the other
    * non-breaking spaces, and includes the C0 file and record separators. Trimming with the wrong
    * set moves chunk boundaries on any text that uses NBSP or ideographic space.
    */
  def isWhite(c: Char): Boolean =
    (c >= 0x09 && c <= 0x0d) || c == 0x85 || Character.isSpaceChar(c)

  /** Horizontal whitespace only — the indentation of a line, not its break. */
  def isIndent(c: Char): Boolean = c == ' ' || c == '\t'

  /** The hardest line break within `text[from, until)`.
    *
    * A blank line is two of the *same* break character, so `\r\n` is one newline rather than two,
    * while `\n\n` and `\r\r` are blank lines.
    */
  def lineBreakOf(text: String, from: Int, until: Int): LineBreak = {
    var hardest = LineBreak.Inline
    var i = from
    while (i < until) {
      val c = text.charAt(i)
      i += 1
      if (c == '\n' || c == '\r') {
        hardest = LineBreak.Newline
        var scanning = true
        while (scanning && i < until) {
          val next = text.charAt(i)
          i += 1
          if (next == '\n' || next == '\r') {
            if (next == c) return LineBreak.Blank
          } else scanning = false
        }
      }
    }
    hardest
  }

  /** The end of `text[from, until)` with trailing whitespace removed. */
  def trimmedEnd(text: String, from: Int, until: Int): Int = {
    var end = until
    while (end > from && isWhite(text.charAt(end - 1))) end -= 1
    end
  }

  /** The start of `text[from, until)` with leading whitespace removed. */
  def trimmedStart(text: String, from: Int, until: Int): Int = {
    var start = from
    while (start < until && isWhite(text.charAt(start))) start += 1
    start
  }

  /** Walk back from `at` over indentation, stopping at `limit`.
    *
    * Used to pull a chunk boundary to the head of its line so that indentation stays attached to
    * the code it indents.
    */
  def startOfIndent(text: String, at: Int, limit: Int): Int = {
    var i = at
    while (i > limit && isIndent(text.charAt(i - 1))) i -= 1
    i
  }
}
