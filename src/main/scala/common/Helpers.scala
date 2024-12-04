package common

import java.nio.file.Paths
import scala.io.Source

object Helpers {
  def readLines(pkg: String, name: String = "input.txt"): Iterator[String] = {
    val path = Paths.get(".").toAbsolutePath
    Source.fromFile(f"$path/src/main/scala/$pkg/$name").getLines()
  }

  def readString(pkg: String, name: String = "input.txt"): String = {
    readLines(pkg).toSeq.mkString("")
  }

  def readArrays(pkg: String, name: String = "input.txt"): Iterator[Array[String]] = {
    readLines(pkg)
      .map(l => l.trim())
      .map(l => l.split("\\s+"))
  }
}
