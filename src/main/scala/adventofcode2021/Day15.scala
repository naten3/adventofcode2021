package adventofcode2021

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day15 {

  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromResource("day15.txt")
      .getLines
      .toArray
      .map(_.map(_.asDigit).toArray)

    println(part1(lines))
    println(part2(lines))
  }

  def doDijkstra(lines: Array[Array[Int]]) = {
    val unvisited = mutable.Set[(Int, Int)]()
    // track touched nodes for determining lowest distance
    val touched = mutable.Set[(Int, Int)]()

    val allUnivisited = lines.zipWithIndex.flatMap({
      case (l, row) => l.zipWithIndex.map({ case (c, col) => (row, col) })
    })
    unvisited.addAll(allUnivisited)
    touched.add(0, 0)

    val endPoint = (lines.length - 1, lines(0).length - 1)
    val distances = lines.zipWithIndex.map({
      case (l, i) =>
        l.zipWithIndex.map({
          case (_, j) => if (i == 0 && j == 0) 0 else Integer.MAX_VALUE
        })
    })

    def getNeighbors(node: (Int, Int)) = {
      val (x, y) = node
      List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter({
        case (r, c) =>
          r >= 0 && c >= 0 && r < lines.length && c < lines(0).length && unvisited
            .contains((r, c))
      })
    }

    def findMinUnvisited() = {
      touched.minBy({ case (y, x) => distances(y)(x) })
    }

    def iterate(startNode: (Int, Int)): Unit = {
      while (!unvisited.isEmpty && unvisited.contains(
               (endPoint._1, endPoint._2)
             )) {
        val currentNode = findMinUnvisited()
        val (y, x) = currentNode

        val currentDistance = distances(y)(x)
        val neighbors = getNeighbors(currentNode)
        neighbors.foreach({
          case (r, c) => {
            val weight = lines(r)(c)
            val possibleNewDistance = currentDistance + weight
            if (possibleNewDistance < distances(r)(c)) {
              distances(r)(c) = possibleNewDistance
              touched.add((r, c))
            }
          }
        })
        unvisited.remove((y, x))
        touched.remove((y, x))
      }
    }

    iterate((0, 0))

    distances(endPoint._1)(endPoint._2)
  }

  def part1(lines: Array[Array[Int]]): Int = {
    doDijkstra(lines)
  }

  def part2(lines: Array[Array[Int]]): Int = {
    val newLines = (0 to 4)
      .flatMap(i => {
        lines.map(
          line =>
            (0 to 4)
              .flatMap(j => line.map(v => (((v + i + j) - 1) % 9) + 1))
              .toArray
        )
      })
      .toArray
    doDijkstra(newLines)
  }

}
