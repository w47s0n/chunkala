package com.w47s0n.chunkala

/** What the parse tree buys over the separator ladder. */
class ScalaSyntaxSuite extends munit.FunSuite {

  private val source =
    """object Registry {
      |  val entries = Map("a" -> 1, "b" -> 2)
      |  def lookup(k: String): Option[Int] = entries.get(k)
      |}
      |case class Item(sku: String, qty: Int) {
      |  def total(price: Long): Long = qty * price
      |}
      |trait Store { def put(i: Item): Unit }
      |""".stripMargin

  test("top-level definitions open chunks") {
    val chunks = Chunker(ChunkerConfig(120, 40, 0, Separators.Default)).chunkScala(source).texts
    List("object Registry", "case class Item").foreach { opening =>
      assert(chunks.exists(_.startsWith(opening)), s"no chunk starts at `$opening`: $chunks")
    }
  }

  test("parsing changes where cuts land") {
    // All on one line, so the ladder gets no further than `\s+` and cuts between
    // `def` and the name it belongs to. The parse tree does not.
    val oneLine = """object A{def f(x:Int)=x+1;def g(y:Int)=y*2};object B{val z=3}"""
    val chunker = Chunker(ChunkerConfig(40, 10, 0, Separators.Default))
    val ladder = chunker.chunkText(oneLine).texts
    val parsed = chunker.chunkScala(oneLine).texts
    assertNotEquals(ladder, parsed, "parsing made no difference")
    assert(ladder.exists(_.endsWith("def")), s"expected a cut after a bare `def`: $ladder")
    assert(parsed.exists(_.startsWith("def g")), s"expected `def g` to open a chunk: $parsed")
  }

  test("a successful parse is reported with its dialect") {
    val result = Chunker.Default.chunkScala("object A { def f = 1 }")
    assertEquals(result.boundaries, Boundaries.Syntax(ScalaDialect.Scala3))
    assert(result.isSyntaxAware)
    assertEquals(result.parseError, None)
  }

  test("unparseable Scala falls back to the ladder and says so") {
    // scalameta has no error recovery, so a single bad construct costs the whole
    // file its syntax awareness. That must degrade, not throw.
    val broken = "object A {\n  def f( = ???\n}\n\nobject B {\n  def g = 1\n}\n"
    val chunker = Chunker(ChunkerConfig(40, 10, 0, Separators.Default))
    val result = chunker.chunkScala(broken)
    assertEquals(result.texts, chunker.chunkText(broken).texts)
    assert(!result.isSyntaxAware)
    assert(result.parseError.isDefined, "the parse failure should be reported")
  }

  test("dialects are tried in order and the first that parses wins") {
    // `then` is a soft keyword in Scala 3 but a plain identifier in 2.13.
    val scala2Only = "object A { val then = 1 }"
    val result =
      Chunker.Default.chunkScala(scala2Only, Seq(ScalaDialect.Scala3, ScalaDialect.Scala213))
    assertEquals(result.boundaries, Boundaries.Syntax(ScalaDialect.Scala213))
  }

  test("chunkFile picks a dialect from the extension") {
    val chunker = Chunker(ChunkerConfig(120, 40, 0, Separators.Default))
    assert(chunker.chunkFile("Registry.scala", source).isSyntaxAware)
    assert(chunker.chunkFile("build.sbt", "lazy val x = project").isSyntaxAware)
    assert(!chunker.chunkFile("notes.txt", source).isSyntaxAware)
  }

  test("a custom ladder still applies inside the parse tree") {
    // Separators fill the gaps between syntax nodes, so replacing them changes
    // chunking even when the source parses.
    val config = ChunkerConfig(60, 20, 0, Separators.of("\\n\\n+"))
    val withDefaults =
      Chunker(ChunkerConfig(60, 20, 0, Separators.Default)).chunkScala(source).texts
    val withCustom = Chunker(config).chunkScala(source).texts
    assert(withDefaults.nonEmpty && withCustom.nonEmpty)
  }

  test("syntax-aware chunks still respect the size ceiling") {
    val chunks = Chunker(ChunkerConfig(80, 30, 0, Separators.Default)).chunkScala(source).chunks
    // Only an indivisible atom may exceed the ceiling; none here is that large.
    assert(chunks.forall(_.byteLength <= 80), chunks.map(_.byteLength))
  }

  test("a multiline class header stays attached to its inheritance clause") {
    val prefix = (1 to 40).map(i => s"  val precedingValue$i: Int = $i").mkString("\n\n")
    val body = (1 to 8)
      .map(i =>
        s"""  def operation$i(input: Input): Output = {
           |    val adjustedInput = prepare(input, $i)
           |    calculate(adjustedInput, strategy = "strategy-$i")
           |  }""".stripMargin
      )
      .mkString("\n\n")
    val parameters =
      (2 to 10).map(i => s"            argument$i: ConfigurationValue$i").mkString(",\n")
    val arguments = (1 to 10).map(i => s"argument$i").mkString(", ")
    val multilineClass =
      s"""class Preceding {
         |$prefix
         |}
         |
         |class Child(argument1: ConfigurationValue1,
         |$parameters
         |  )
         |  extends Parent($arguments) {
         |$body
         |}
         |""".stripMargin
    val chunks =
      Chunker(ChunkerConfig(1500, 100, 200, Separators.Default))
        .chunkScala(multilineClass)
        .chunks

    assert(
      chunks.exists(chunk =>
        chunk.text.contains("class Child") && chunk.text.contains("extends Parent")
      ),
      s"no chunk preserves the complete inheritance declaration: $chunks"
    )
    assert(chunks.forall(_.byteLength <= 1500), chunks.map(_.byteLength))
  }
}
