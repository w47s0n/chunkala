package com.w47s0n.chunkala.internal

/** UTF-8 byte offsets for a UTF-16 string, precomputed once.
  *
  * Sizes are configured in bytes but the text is indexed in Java chars, so every size comparison in
  * the algorithm needs this conversion. Doing it per query would make the merge pass quadratic in
  * the text length; one O(n) table makes each query a subtraction.
  */
private[chunkala] final class ByteIndex(text: String) {

  /** `prefix(i)` is the UTF-8 length of `text.take(i)`. */
  private val prefix: Array[Int] = {
    val table = new Array[Int](text.length + 1)
    var i = 0
    while (i < text.length) {
      val c = text.charAt(i)
      val width =
        if (c < 0x80) 1
        else if (c < 0x800) 2
        // A surrogate pair is one 4-byte character. Charging it entirely to the low half keeps the
        // table non-decreasing and leaves the offset for the pair's own index equal to the offset
        // before it, which is what callers holding a char index into the middle of a pair would
        // expect.
        else if (Character.isHighSurrogate(c)) 0
        else if (Character.isLowSurrogate(c)) 4
        else 3
      table(i + 1) = table(i) + width
      i += 1
    }
    table
  }

  /** The UTF-8 offset of a char index. */
  def byteAt(charIndex: Int): Int = prefix(charIndex)

  /** The UTF-8 length of `text.substring(from, until)`. */
  def lengthOf(from: Int, until: Int): Int = prefix(until) - prefix(from)

  /** The UTF-8 length of the whole text. */
  val totalBytes: Int = prefix(text.length)
}
