package adventofcode2021

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day5 {
  type PuzzleInput = List[((Int, Int), (Int, Int))]

  def processInput(lines: Seq[String]): PuzzleInput = {
    val inputRegex = raw"^([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)$$".r
    lines
      .map({
        case inputRegex(a, b, c, d) =>
          (
            (Integer.parseInt(a), Integer.parseInt(b)),
            (Integer.parseInt(c), Integer.parseInt(d))
          )
      })
      .toList
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day5.txt").getLines.toSeq

    val input = processInput(lines);

    println(part1(input))
    println(part2(input))
  }

  class PointArray(input: PuzzleInput) {
    val flattenedInput: Seq[(Int, Int)] =
      input.flatMap(line => List(line._1, line._2))
    val startValue = flattenedInput.head
    val (xMin, xMax, yMin, yMax) = flattenedInput.tail.foldLeft(
      (startValue._1, startValue._1, startValue._2, startValue._2)
    )((extrema, newVal) => {
      val xMin = Math.min(newVal._1, extrema._1)
      val xMax = Math.max(newVal._1, extrema._2)
      val yMin = Math.min(newVal._2, extrema._3)
      val yMax = Math.max(newVal._2, extrema._4)
      (xMin, xMax, yMin, yMax)
    })

    val array = Array.ofDim[Int](xMax - xMin + 1, yMax - yMin + 1)

    def addLine(line: ((Int, Int), (Int, Int))) = {
      line match {
        case ((x1, y1), (x2, y2)) => {
          // transform points to start at 0
          val ((xn1, yn1), (xn2, yn2)) =
            ((x1 - xMin, y1 - yMin), (x2 - xMin, y2 - yMin))

          if (xn1 == xn2 || yn1 == yn2) {
            // it's a horizontal or vertical line
            for (x <- Math.min(xn1, xn2) to Math.max(xn1, xn2);
                 y <- Math.min(yn1, yn2) to Math.max(yn1, yn2)) {
              array(y)(x) = array(y)(x) + 1
            }
          } else {
            // has a slope of 1 or -1
            val positiveSlope = ((yn2 - yn1) / (xn2 - xn1)) > 0
            val (leftX, leftY) = Seq((xn1, yn1), (xn2, yn2)).minBy(_._1)
            for (i <- 0 to Math.abs(xn1 - xn2)) {
              val x = leftX + i
              val y = leftY + (if (positiveSlope) i else -1 * i)
              array(y)(x) = array(y)(x) + 1
            }
          }
        }
      }
    }

    def getMultilinePointCount = array.flatten.count(_ > 1)
  }

  def part1(input: PuzzleInput): Int = {
    val array = new PointArray(input)

    input
      .filter({ case ((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2 })
      .foreach(array.addLine)

    array.getMultilinePointCount
  }

  def part2(input: PuzzleInput): Int = {
    val array = new PointArray(input)

    input
      .foreach(array.addLine)
    array.getMultilinePointCount
  }
}
