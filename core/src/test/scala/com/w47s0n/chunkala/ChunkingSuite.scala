package com.w47s0n.chunkala

/** The reference implementation's own tests, ported.
  *
  * chunkala is a port of CocoIndex's `RecursiveSplitter`; these cases come from that
  * implementation's test suite and pin the behaviour that must not drift.
  */
class ChunkingSuite extends munit.FunSuite {

  private def chunk(
      text: String,
      targetSize: Int,
      minSize: Int,
      overlap: Int = 0
  ): Vector[String] =
    Chunker(ChunkerConfig(targetSize, minSize, overlap, Separators.Default)).chunkText(text).texts

  private def utf8(s: String): Int = s.getBytes("UTF-8").length

  test("splits on the strongest available boundary") {
    assertEquals(
      chunk("Linea 1.\nLinea 2.\n\nLinea 3.", 15, 5),
      Vector("Linea 1.", "Linea 2.", "Linea 3.")
    )
  }

  test("a long run of prose stays under the ceiling") {
    val chunks = chunk("A very very long text that needs to be split.", 20, 12)
    assert(chunks.length > 1, chunks)
    assert(chunks.forall(utf8(_) <= 20), chunks)
  }

  test("overlap keeps chunks within a tolerance of the ceiling") {
    val chunks =
      chunk("This is a test text that is a bit longer to see how the overlap works.", 20, 10, 5)
    assert(chunks.length > 1, chunks)
    assert(chunks.forall(utf8(_) <= 25), chunks)
  }

  test("boundary whitespace is trimmed away") {
    val chunks = chunk("  \n First chunk  \n\n  Second chunk with spaces at the end    \n", 30, 10)
    assertEquals(chunks.length, 3, chunks)
    assert(!chunks.head.startsWith("  "), chunks)
  }

  test("positions are 1-based and track lines") {
    val chunks =
      Chunker(ChunkerConfig(10, 5, 0, Separators.Default)).chunkText("Chunk1\n\nChunk2").chunks
    assertEquals(chunks.length, 2)
    assertEquals((chunks(0).start.line, chunks(0).start.column), (1, 1))
    assertEquals((chunks(1).start.line, chunks(1).start.column), (3, 1))
  }

  test("a custom separator ladder replaces the default") {
    val config = ChunkerConfig(10, 4, 0, Separators.of("---"))
    assertEquals(
      Chunker(config).chunkText("Part1---Part2---Part3").texts,
      Vector("Part1", "Part2", "Part3")
    )
  }

  test("sizes are UTF-8 bytes, not characters") {
    // 21 characters but 63 bytes, so a 40-byte ceiling has to cut it in two.
    val chunks = Chunker(ChunkerConfig(40, 12, 0, Separators.Default))
      .chunkText("一二三四五六七八九十。一二三四五六七八九十。")
      .chunks
    assertEquals(chunks.map(_.text), Vector("一二三四五六七八九十", "一二三四五六七八九十"))
    assertEquals(chunks(1).start.byte, 33)
    assertEquals(chunks(1).start.char, 11)
  }

  test("empty input yields no chunks") {
    assertEquals(Chunker.Default.chunkText("").chunks, Vector.empty[Chunk])
  }

  test("whitespace-only input yields no chunks") {
    assertEquals(Chunker.Default.chunkText("   \n\n  \t \n").chunks, Vector.empty[Chunk])
  }

  test("chunk text is exactly the source it points at") {
    val source = "Alpha one.\nBeta two.\n\nGamma three. Delta four."
    Chunker(ChunkerConfig(20, 8, 0, Separators.Default)).chunkText(source).chunks.foreach { c =>
      assertEquals(c.text, source.substring(c.start.char, c.end.char))
      assertEquals(c.byteLength, utf8(c.text))
    }
  }

  test("chunks come back in source order") {
    val chunks = Chunker(ChunkerConfig(40, 15, 0, Separators.Default))
      .chunkText(("sentence one. " * 30).trim)
      .chunks
    assert(chunks.sliding(2).forall(w => w.head.start.byte <= w(1).start.byte), "not ordered")
  }
}
