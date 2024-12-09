package task5

import common.Helpers
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break

@main
def main(): Unit = {
  val (order, input) = readInput(Helpers.readLines("task5"))
  println(solve1(order, input))
  println(solve2(order, input))
}

def readInput(input: Iterator[String]): (Map[Int, Set[Int]], Seq[Array[Int]]) = {
  val first = ArrayBuffer[(Int, Int)]()
  val second = ArrayBuffer[Array[Int]]()
  var inSecondPart = false
  for (line <- input) {
    if (line.isBlank) {
      inSecondPart = true
    } else if (!inSecondPart) {
      val twoValues = line.split('|').map(_.toInt)
      first.addOne((twoValues(0), twoValues(1)))
    } else {
      second += line.split(',').map(_.toInt)
    }
  }

  (toNeighboursMap(first.toSeq), second.toSeq)
}

def toNeighboursMap(input: Seq[(Int, Int)]): Map[Int, Set[Int]] = {
  val neighbours = input.flatMap{case (a, b) => Seq(a, b)}
    .map(_ -> scala.collection.mutable.Set[Int]())
    .toMap

  input.foreach((curr, next) => {
    neighbours(curr) += next
  })
  neighbours.map((k, v) => (k, v.toSet))
}

def isLineOk(line: Seq[Int], order: Map[Int, Set[Int]]): Boolean = {
  line.indices.forall { l =>
    (l + 1 until line.length).forall { r =>
      !order.getOrElse(line(r), Set.empty).contains(line(l))
    }
  }
}

def fixLineOrder(line: Array[Int], order: Map[Int, Set[Int]]): Array[Int] = {
  for (l <- line.indices) {
    for (r <- l + 1 until line.length) {
      if (order.contains(line(r)) && order(line(r)).contains(line(l))) {
        val prevR = line(r)
        line(r) = line(l)
        line(l) = prevR
      }
    }
  }
  line
}


def solve1(order: Map[Int, Set[Int]], lines: Seq[Array[Int]]): Int = {
  lines.filter(line => isLineOk(line, order))
    .map(line => line(line.length >> 1))
    .sum
}


def solve2(order: Map[Int, Set[Int]], lines: Seq[Array[Int]]): Int = {
  lines.filter(line => !isLineOk(line, order))
    .map(line => fixLineOrder(line, order))
    .map(line => line(line.length >> 1))
    .sum
}

