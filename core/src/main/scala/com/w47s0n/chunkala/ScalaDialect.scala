package com.w47s0n.chunkala

import scala.meta.{dialects, Dialect}

/** A Scala dialect to parse with.
  *
  * Wrapping scalameta's `Dialect` rather than exposing it keeps the published API independent of
  * scalameta's. Only the dialects worth chunking are named; adding another is a new case here and
  * stays binary-compatible.
  */
enum ScalaDialect(private[chunkala] val underlying: Dialect) {
  case Scala3 extends ScalaDialect(dialects.Scala3)
  case Scala213 extends ScalaDialect(dialects.Scala213)
  case Scala212 extends ScalaDialect(dialects.Scala212)
  case Sbt extends ScalaDialect(dialects.Sbt1)
}

object ScalaDialect {

  /** What to try when the caller does not name a dialect.
    *
    * Scala 3 is close enough to a superset in practice that it goes first; 2.13 catches what it
    * genuinely rejects, at the cost of one reparse on those files. Dialects are tried in order and
    * the first that parses wins.
    */
  val Auto: Vector[ScalaDialect] = Vector(Scala3, Scala213)

  /** Look up a dialect by name: `"scala3"`, `"scala213"`, `"scala212"`, `"sbt"`. */
  def byName(name: String): Option[ScalaDialect] = {
    val wanted = name.trim.toLowerCase.replace("-", "").replace("_", "")
    values.find(_.toString.toLowerCase == wanted)
  }

  /** Which dialects suit a filename, or `None` if it does not look like Scala.
    *
    * Only the extension is consulted, so a path or a bare name both work.
    */
  def forFile(filename: String): Option[Vector[ScalaDialect]] = {
    val name = filename.substring(filename.lastIndexOf('/') + 1)
    val dot = name.lastIndexOf('.')
    val extension = if (dot < 0) "" else name.substring(dot + 1).toLowerCase
    extension match {
      case "scala" | "sc" => Some(Auto)
      case "sbt" => Some(Vector(Sbt))
      case _ => None
    }
  }
}
