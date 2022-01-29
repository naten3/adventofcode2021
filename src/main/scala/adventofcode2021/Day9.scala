package adventofcode2021

import scala.collection.mutable
import scala.io.Source

object Day9 {
  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromResource("day9.txt")
      .getLines
      .map(_.toCharArray.map(_.asDigit).toArray)
      .toArray

    val height = lines.length
    val width = lines(0).length

    def getNeighbors(row: Int, column: Int) = {
      List(
        (row - 1, column),
        (row + 1, column),
        (row, column - 1),
        (row, column + 1)
      ).filter({
        case (r, c) => r >= 0 && r < height && c >= 0 && c < width
      })
    }

    def part1(): Int = {

      (0 until height)
        .map(
          row =>
            (0 until width)
              .map(column => {
                val positionValue = lines(row)(column)
                val isLowPoint = getNeighbors(row, column)
                  .forall({ case (r, c) => lines(r)(c) > positionValue })
                if (isLowPoint) {
                  positionValue + 1
                } else {
                  0
                }
              })
              .sum
        )
        .sum
    }

    def getBasinSize(row: Int, col: Int) = {
      val visited = mutable.Set[(Int, Int)]()
      visited.add((row, col))
      val workQueue = mutable.Queue[(Int, Int)]()
      workQueue.enqueue((row, col))
      while (!workQueue.isEmpty) {
        val (row, col) = workQueue.dequeue()
        val neighbors = getNeighbors(row, col)
          .filter({
            case (r, c) => lines(r)(c) < 9 && !visited.contains((r, c))
          })
        workQueue.enqueueAll(neighbors)
        visited.addAll(neighbors)
      }
      visited.size
    }

    def part2(): Int = {
      (0 until height)
        .flatMap(
          row =>
            (0 until width)
              .map(column => {
                val positionValue = lines(row)(column)
                val isLowPoint = getNeighbors(row, column)
                  .forall({ case (r, c) => lines(r)(c) > positionValue })
                if (isLowPoint) {
                  getBasinSize(row, column)
                } else {
                  0
                }
              })
        )
        .filter(_ > 0)
        .sorted
        .reverse
        .slice(0, 3)
        .product
    }

    println(part1)
    println(part2)
  }

}
