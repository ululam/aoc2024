package task2

import common.Helpers
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

@main
def main(): Unit = {
  val data = Helpers.readArrays("task2")
    .map(a => a.map(v => v.toInt))
//  val answer1 = data.map(a => isSafe(a)).sum
//  println(answer1)
  val answer2 = data.map(a => isSafe2(a)).sum
  println(answer2)
}

def isSafe2(line: Array[Int]): Int = {
  if (isSafe(line) == 1) return 1
  val buf: ArrayBuffer[Int] = ArrayBuffer()
  buf.addAll(line)
  for (i <- line.indices) {
    val el = buf.remove(i)
    if (isSafe(buf.toArray) == 1) {
      return 1
    }
    buf.insert(i, el)
  }
  0
}

def isSafe(line: Array[Int]): Int = {
  val sign = Math.signum(line(1) - line(0))
  for (i <- 1 until line.length) {
    val delta = sign * (line(i) - line(i - 1))
    if (delta < 1 || delta > 3) {
      return 0
    }
  }
  1
}

