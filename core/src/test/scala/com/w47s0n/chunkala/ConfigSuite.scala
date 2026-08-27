package com.w47s0n.chunkala

class ConfigSuite extends munit.FunSuite {

  test("the single-argument config derives a minimum and no overlap") {
    val config = ChunkerConfig(1000)
    assertEquals(config.targetSize, 1000)
    assertEquals(config.minSize, 500)
    assertEquals(config.overlap, 0)
  }

  test("overlap is capped at the minimum chunk size") {
    val config = ChunkerConfig(1000, 200, 500, Separators.Default)
    assertEquals(config.overlap, 500, "the requested value is kept as asked")
    assertEquals(config.effectiveOverlap, 200, "but only the minimum can be delivered")
  }

  test("nonsensical sizes are rejected at construction") {
    intercept[IllegalArgumentException](ChunkerConfig(0))
    intercept[IllegalArgumentException](ChunkerConfig(-1))
    intercept[IllegalArgumentException](ChunkerConfig(100, -1, 0, Separators.Default))
    intercept[IllegalArgumentException](ChunkerConfig(100, 200, 0, Separators.Default))
    intercept[IllegalArgumentException](ChunkerConfig(100, 50, -1, Separators.Default))
  }

  test("copy re-validates") {
    intercept[IllegalArgumentException](ChunkerConfig(100).copy(minSize = 500))
  }

  test("an invalid separator is reported rather than thrown") {
    val result = Separators.from(Seq("fine", "[unclosed"))
    assert(result.isLeft, result)
    assertEquals(result.left.toOption.map(_.pattern), Some("[unclosed"))
  }

  test("Separators.of throws, for patterns written at the call site") {
    intercept[IllegalArgumentException](Separators.of("[unclosed"))
  }

  test("an empty ladder means never cut on separators") {
    val config = ChunkerConfig(10, 5, 0, Separators.from(Nil).toOption.get)
    assertEquals(Chunker(config).chunkText("one two three four five").size, 1)
  }

  test("the default ladder is Unicode-aware") {
    // NBSP is Unicode whitespace but not ASCII whitespace; `\s` must match it or
    // the last rung cannot break this text at all.
    val chunks = Chunker(ChunkerConfig(12, 4, 0, Separators.Default))
      .chunkText("alpha beta gamma delta")
      .texts
    assert(chunks.length > 1, chunks)
  }
}
