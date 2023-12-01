package io

import scala.io.Source

object io {

  def load(day: String)(run: List[String] => Unit): Unit = {
    val filename = s"src/main/resources/$day"
    val input = Source.fromFile(filename)
    try {
      val lines = input.getLines().toList
      run(lines)
    } finally {
      input.close()
    }
  }

}
