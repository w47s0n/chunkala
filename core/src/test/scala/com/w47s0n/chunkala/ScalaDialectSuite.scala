package com.w47s0n.chunkala

class ScalaDialectSuite extends munit.FunSuite {

  test("dialects resolve by name, case and separators ignored") {
    assertEquals(ScalaDialect.byName("scala3"), Some(ScalaDialect.Scala3))
    assertEquals(ScalaDialect.byName("Scala213"), Some(ScalaDialect.Scala213))
    assertEquals(ScalaDialect.byName("scala-212"), Some(ScalaDialect.Scala212))
    assertEquals(ScalaDialect.byName("SBT"), Some(ScalaDialect.Sbt))
    assertEquals(ScalaDialect.byName("python"), None)
  }

  test("filenames map to the dialects worth trying") {
    assertEquals(ScalaDialect.forFile("src/main/scala/Main.scala"), Some(ScalaDialect.Auto))
    assertEquals(ScalaDialect.forFile("worksheet.sc"), Some(ScalaDialect.Auto))
    assertEquals(ScalaDialect.forFile("build.sbt"), Some(Vector(ScalaDialect.Sbt)))
    assertEquals(ScalaDialect.forFile("notes.txt"), None)
    assertEquals(ScalaDialect.forFile("Makefile"), None)
  }

  test("extension matching ignores case and directory names") {
    assertEquals(ScalaDialect.forFile("a.scala/b.txt"), None)
    assertEquals(ScalaDialect.forFile("A/B/Main.SCALA"), Some(ScalaDialect.Auto))
  }
}
