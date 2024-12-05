package task4

import common.Helpers
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer


@main
def main(): Unit = {
  val matrix: Array[ArrayBuffer[Char]] = Helpers.readLines("task4")
    .map(line => line.toCharArray)
    .map(arr => ArrayBuffer.from(arr))
    .toArray

  println(solve(matrix))
  println(solve2(matrix))
}

def solve(matrix: Array[ArrayBuffer[Char]]): Int = {
  val moves = Array(
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1)
  )

  var counter = 0
  @tailrec
  def bt(word: String, pos: (Int, Int), move: (Int, Int)): Unit = {
    if (word.isEmpty) {
      counter += 1
      return
    }
    val (i, j) = (pos._1 + move._1, pos._2 + move._2)
    if (isInBounds((i, j)) && matrix(i)(j) == word.charAt(0)) {
      bt(word.substring(1), (i, j), move)
    }
  }

  def isInBounds(pos: (Int, Int)): Boolean = {
    0 <= pos._1 && pos._1 < matrix.length && 0 <= pos._2 && pos._2 < matrix(0).length
  }

  val target = "XMAS"
  matrix.indices.foreach(i => matrix(i).indices.foreach(j => 
    if (matrix(i)(j) == target.charAt(0)) {
      moves.foreach(move => bt(target.substring(1), (i, j), move))
    }
  ))
  counter
}

def solve2(matrix: Array[ArrayBuffer[Char]]): Int = {
  val possibleXCorners = Set("MSSM", "SMMS", "SSMM", "MMSS")
  def isX_Mas(pos: (Int, Int)): Int = {
    val (r, c) = pos
    if (r > matrix.length - 3) return 0
    if (c > matrix(0).length - 3) return 0
    if (matrix(r+1)(c+1) != 'A') return 0
    val corners = "" + matrix(r)(c) + matrix(r+2)(c) + matrix(r+2)(c+2) + matrix(r)(c+2)
    if (!possibleXCorners.contains(corners)) return 0
    1
  }

  matrix.indices
    .map(i => matrix(i).indices.map(j => isX_Mas(i,j)))
    .map(row => row.sum)
    .sum
}
