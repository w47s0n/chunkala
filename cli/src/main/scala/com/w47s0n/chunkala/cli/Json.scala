package com.w47s0n.chunkala.cli

/** Just enough JSON to print a chunk, so the CLI needs no dependency for it. */
private[cli] object Json {

  def string(value: String): String = {
    val out = StringBuilder("\"")
    value.foreach {
      case '"' => out ++= "\\\""
      case '\\' => out ++= "\\\\"
      case '\n' => out ++= "\\n"
      case '\r' => out ++= "\\r"
      case '\t' => out ++= "\\t"
      case c if c < 0x20 => out ++= f"\\u${c.toInt}%04x"
      case c => out += c
    }
    (out += '"').result()
  }
}
