package task3

import common.Helpers
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

@main
def main(): Unit = {
  val answer1 = solve(Helpers.readString("task3")) 
  println(answer1)
  
  val answer2 = ("do()" + Helpers.readString("task3"))
    .split("(?=do\\(\\)|don't\\(\\))")
    .map {
      case s"do()$rest" => solve(rest)
      case _ => 0
    }.sum()

  println(answer2)
}

def solve(line: String): Int = {
    """mul\((\d+),(\d+)\)""".r
      .findAllIn(line)
      .map(g => g.replace("mul(", "").replace(")","").split(","))
      .map(a => a(0).toInt * a(1).toInt)
      .sum
}

