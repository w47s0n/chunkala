package com.w47s0n.chunkala

/** The README's examples, compiled and run.
  *
  * Documentation that no longer compiles is worse than none, and a chunking library's README is
  * mostly API calls.
  */
class ReadmeSuite extends munit.FunSuite {

  private val source = "object Registry {\n  def lookup(k: String) = k\n}\n"

  test("the quick-start example") {
    val chunker = Chunker(targetSize = 2000)
    val result = chunker.chunkScala(source)
    result.chunks.foreach { chunk =>
      assert(chunk.byteLength > 0)
      assert(chunk.start.line <= chunk.end.line)
    }
  }

  test("the four entry points") {
    val chunker = Chunker(targetSize = 2000)
    assert(chunker.chunkScala(source).nonEmpty)
    assert(chunker.chunkScala(source, Seq(ScalaDialect.Scala213)).nonEmpty)
    assert(chunker.chunkFile("build.sbt", "lazy val x = project").nonEmpty)
    assert(chunker.chunkText("some prose here").nonEmpty)
  }

  test("matching on boundaries") {
    val result = Chunker.Default.chunkFile("Registry.scala", source)
    val described = result.boundaries match {
      case Boundaries.Syntax(dialect) => s"parsed as $dialect"
      case Boundaries.Separators => "no parse attempted"
      case Boundaries.ParseFailed(err) => s"did not parse: $err"
    }
    assertEquals(described, s"parsed as ${ScalaDialect.Scala3}")
  }

  test("the configuration example") {
    val config = ChunkerConfig(
      targetSize = 2000,
      minSize = 500,
      overlap = 200,
      separators = Separators.Default
    )
    assertEquals(config.effectiveOverlap, 200)
    assert(Chunker(config).chunkScala(source).nonEmpty)
  }

  test("both ways of building a ladder") {
    assertEquals(Separators.of("\\n\\n+", "\\n", "[.!?]\\s+").size, 3)
    assert(Separators.from(Seq("\\n")).isRight)
  }
}
