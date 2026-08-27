package com.w47s0n.chunkala.cli

import com.w47s0n.chunkala.*
import java.io.{BufferedWriter, OutputStreamWriter, PrintWriter}

/** Emits chunk offsets for a batch of cases, so they can be diffed against the reference
  * implementation, CocoIndex.
  *
  * One process handles a whole sweep, because JVM startup dwarfs the chunking.
  *
  * Each input line is `targetSize \t minSize \t overlap \t separators \t dialect \t path`, where an
  * empty `minSize` means "half the ceiling", an empty `separators` means the default ladder
  * (otherwise rungs are joined with U+0001), and an empty `dialect` means do not parse.
  */
private[cli] object Parity {

  private val RungSeparator = 1.toChar

  def run(casesFile: String): Unit = {
    val out = PrintWriter(BufferedWriter(OutputStreamWriter(System.out), 1 << 16))
    Main.readFile(casesFile).linesIterator.zipWithIndex.foreach { (line, caseIndex) =>
      val Array(target, min, overlap, rungs, dialect, path) = line.split("\t", -1): @unchecked
      val ladder =
        if (rungs.isEmpty) Separators.Default
        else Separators.of(rungs.split(RungSeparator).toIndexedSeq*)
      val config = ChunkerConfig(
        targetSize = target.toInt,
        minSize = if (min.isEmpty) target.toInt / 2 else min.toInt,
        overlap = overlap.toInt,
        separators = ladder
      )
      val source = Main.readFile(path)
      val chunker = Chunker(config)
      val result =
        if (dialect.isEmpty) chunker.chunkText(source)
        else
          ScalaDialect
            .byName(dialect)
            .fold(chunker.chunkText(source))(d => chunker.chunkScala(source, Seq(d)))

      result.chunks.zipWithIndex.foreach { (c, chunkIndex) =>
        out.println(
          s"$caseIndex\t$chunkIndex\t${c.start.byte}\t${c.end.byte}\t" +
            s"${c.start.char}\t${c.end.char}\t" +
            s"${c.start.line}\t${c.start.column}\t${c.end.line}\t${c.end.column}"
        )
      }
    }
    out.flush()
  }
}
