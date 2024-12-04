package task1

import common.Helpers
import java.nio.file.Paths
import scala.io.Source

@main
def main(): Unit = {
  val path = Paths.get(".").toAbsolutePath
  val (left, right) = Helpers.readArrays("task1")
    .map(v => (v(0).toInt, v(1).toInt))
    .toSeq.unzip

 println(part1(left, right))
 println(part2(left, right))
}

def part1(left: Seq[Int], right: Seq[Int]): Int = {
  (left.sorted zip right.sorted).map((a,b) => Math.abs(a-b)).sum
}

def part2(left: Seq[Int], right: Seq[Int]): Int = {
  val freqMap = right.groupBy(e => e).map(e => (e._1, e._2.length))
  left.map(v => freqMap.getOrElse(v, 0) * v).sum
}
