package adventofcode2021

import scala.collection.mutable
import scala.io.Source

object Day11 {
  type OctopusArray = Array[Array[Int]]

  def readSource(): OctopusArray =
    Source
      .fromResource("day11.txt")
      .getLines
      .toList
      .map(_.map(_.asDigit).toArray)
      .toArray

  def main(args: Array[String]): Unit = {

    println(part1(readSource()))
    println(part2(readSource()))
  }

  def getNeighbors(lines: OctopusArray, row: Int, col: Int) =
    List(
      (row + 1, col),
      (row - 1, col),
      (row, col - 1),
      (row, col + 1),
      (row + 1, col + 1),
      (row + 1, col - 1),
      (row - 1, col + 1),
      (row - 1, col - 1)
    ).filter({
      case (r, c) => r >= 0 && r < lines.length && c >= 0 && c < lines(0).length
    })

  def doOctopusStep(lines: OctopusArray): Int = {
    var flashCount = 0
    val height = lines.length
    val width = lines(0).length
    var all9s = mutable.Set[(Int, Int)]()
    (0 until height).foreach(
      row =>
        (0 until width).foreach(col => {
          val prevVal = lines(row)(col)
          val nextVal = prevVal + 1
          lines(row)(col) = nextVal
          if (nextVal > 9) {
            all9s.add(((row, col)))
          }
        })
    )
    var hasFlashed = !all9s.isEmpty
    var previous9s = all9s
    while (hasFlashed) {
      hasFlashed = false
      val next9s = mutable.Set[(Int, Int)]()
      previous9s.foreach({
        case (row, col) => {
          getNeighbors(lines, row, col)
            .filter(!all9s.contains(_))
            .foreach({
              case (neighborRow, neighborCol) =>
                val neighborVal = lines(neighborRow)(neighborCol)
                val nextVal = neighborVal + 1
                if (nextVal == 10) {
                  hasFlashed = true
                  next9s.add((neighborRow, neighborCol))
                }
                lines(neighborRow)(neighborCol) = nextVal
            })
        }
      })
      all9s.addAll(next9s)
      previous9s = next9s
    }
    all9s.foreach({ case (r, c) => lines(r)(c) = 0 })
    //lines.foreach(l => System.out.println(l.mkString))
    //System.out.println("\n")
    all9s.size
  }

  def part1(lines: OctopusArray): Int = {
    (1 to 100).map(_ => doOctopusStep(lines)).sum
  }

  def part2(lines: OctopusArray): Int = {
    val arSize = lines.length * lines(0).length
    Iterator.from(1).find(_ => doOctopusStep(lines) == arSize).get
  }

}
