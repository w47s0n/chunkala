package com.w47s0n.chunkala.internal

import com.w47s0n.chunkala.{Chunk, Position}

/** Phase three: turn char-index spans into chunks carrying full positions.
  *
  * Line and column need a scan of the text, so rather than scanning per boundary, the distinct
  * boundaries are visited in ascending order and a single cursor is advanced through the source
  * once.
  */
private[chunkala] object Locator {

  def locate(text: String, bytes: ByteIndex, spans: Vector[Span]): Vector[Chunk] = {
    val edges = spans.flatMap(span => Vector(span.start, span.end)).distinct.sorted
    val positions = edges
      .foldLeft((Map.empty[Int, Position], Cursor.Start)) { case ((found, cursor), edge) =>
        val moved = cursor.advanceTo(edge, text)
        (found.updated(edge, moved.positionIn(bytes)), moved)
      }
      ._1
    spans.map { span =>
      Chunk(text.substring(span.start, span.end), positions(span.start), positions(span.end))
    }
  }

  /** A position part-way through the source, in every unit at once.
    *
    * @param char
    *   index in UTF-16 code units, which is how the source is indexed
    * @param codePoints
    *   how many Unicode code points precede it
    */
  private final case class Cursor(char: Int, codePoints: Int, line: Int, column: Int) {

    def positionIn(bytes: ByteIndex): Position =
      Position(byte = bytes.byteAt(char), char = codePoints, line = line, column = column)

    /** Move forward to `target`, which must not be behind the cursor. */
    def advanceTo(target: Int, text: String): Cursor = {
      var index = char
      var points = codePoints
      var currentLine = line
      var currentColumn = column
      while (index < target && index < text.length) {
        val codePoint = text.codePointAt(index)
        index += Character.charCount(codePoint)
        points += 1
        if (codePoint == '\n') {
          currentLine += 1
          currentColumn = 1
        } else currentColumn += 1
      }
      // A target past the end of the text clamps to the end, which is what the final sentinel
      // boundary needs.
      Cursor(target min text.length max index, points, currentLine, currentColumn)
    }
  }

  private object Cursor {
    val Start: Cursor = Cursor(char = 0, codePoints = 0, line = 1, column = 1)
  }
}
