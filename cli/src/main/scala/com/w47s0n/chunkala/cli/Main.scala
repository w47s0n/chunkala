package com.w47s0n.chunkala.cli

import com.w47s0n.chunkala.*
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, StandardCharsets}
import java.nio.file.{Files, Path}

/** A thin driver for the library: chunk a file or stdin, print JSON.
  *
  * Not published — it exists so the library can be exercised from a shell and so the parity harness
  * has something to drive.
  */
object Main {

  private val Usage =
    """usage: chunkala [options] [file]
      |  --target-size N    size ceiling in UTF-8 bytes (default 1000)
      |  --min-size N       minimum chunk size (default: half the ceiling)
      |  --overlap N        bytes shared between consecutive chunks (default 0)
      |  --separators RE    repeatable; replaces the default separator ladder
      |  --dialect D        scala3 | scala213 | scala212 | sbt | none.
      |                     Inferred from the filename when a file is given;
      |                     stdin defaults to Scala, "none" disables parsing.
      |  --dialects         list the dialects understood
      |
      |Reads stdin when no file is given. Writes a JSON array of chunks to stdout.""".stripMargin

  def main(args: Array[String]): Unit =
    args.toList match {
      case "--parity" :: cases :: Nil => Parity.run(cases)
      case "--dialects" :: _ =>
        println(ScalaDialect.values.map(_.toString.toLowerCase).mkString(" "))
      case ("-h" | "--help") :: _ => println(Usage)
      case other => chunk(parse(other))
    }

  private final case class Options(
      targetSize: Int = 1000,
      minSize: Option[Int] = None,
      overlap: Int = 0,
      separators: Vector[String] = Vector.empty,
      dialect: Option[String] = None,
      file: Option[String] = None
  ) {
    def config: ChunkerConfig = {
      val ladder =
        if (separators.isEmpty) Separators.Default
        else Separators.from(separators).fold(error => fail(error.toString), identity)
      ChunkerConfig(targetSize, minSize.getOrElse(targetSize / 2), overlap, ladder)
    }
  }

  @annotation.tailrec
  private def parse(args: List[String], options: Options = Options()): Options =
    args match {
      case Nil => options
      case "--target-size" :: v :: rest => parse(rest, options.copy(targetSize = int(v)))
      case "--min-size" :: v :: rest => parse(rest, options.copy(minSize = Some(int(v))))
      case "--overlap" :: v :: rest => parse(rest, options.copy(overlap = int(v)))
      case "--separators" :: v :: rest =>
        parse(rest, options.copy(separators = options.separators :+ v))
      case "--dialect" :: v :: rest => parse(rest, options.copy(dialect = Some(v)))
      case flag :: Nil if flag.startsWith("--") => fail(s"$flag needs a value\n$Usage")
      case flag :: _ if flag.startsWith("-") => fail(s"unknown option: $flag\n$Usage")
      case path :: rest => parse(rest, options.copy(file = Some(path)))
    }

  private def chunk(options: Options): Unit = {
    val source = options.file.fold(readUtf8(System.in.readAllBytes()))(readFile)
    val chunker = Chunker(options.config)
    val result = options.dialect match {
      case Some("none") => chunker.chunkText(source)
      case Some(name) =>
        ScalaDialect.byName(name) match {
          case Some(dialect) => chunker.chunkScala(source, Seq(dialect))
          case None => chunker.chunkText(source)
        }
      case None => options.file.fold(chunker.chunkScala(source))(chunker.chunkFile(_, source))
    }
    println(render(result))
  }

  private def render(result: Chunking): String =
    result.chunks
      .map { c =>
        s"""  {"start_byte": ${c.start.byte}, "end_byte": ${c.end.byte}, """ +
          s""""start_char": ${c.start.char}, "end_char": ${c.end.char}, """ +
          s""""start_line": ${c.start.line}, "start_column": ${c.start.column}, """ +
          s""""end_line": ${c.end.line}, "end_column": ${c.end.column}, """ +
          s""""text": ${Json.string(c.text)}}"""
      }
      .mkString("[\n", ",\n", "\n]")

  private def int(value: String): Int =
    value.toIntOption.getOrElse(fail(s"expected a number, got `$value`"))

  private[cli] def readFile(path: String): String = readUtf8(Files.readAllBytes(Path.of(path)))

  /** Decode strictly: byte offsets are only meaningful for well-formed UTF-8, and silently
    * substituting replacement characters would shift every one of them.
    */
  private[cli] def readUtf8(bytes: Array[Byte]): String =
    try StandardCharsets.UTF_8.newDecoder().decode(ByteBuffer.wrap(bytes)).toString
    catch { case e: CharacterCodingException => fail(s"input is not valid UTF-8: ${e.getMessage}") }

  private def fail(message: String): Nothing = {
    Console.err.println(message)
    sys.exit(2)
  }
}
