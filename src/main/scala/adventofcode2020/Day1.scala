package adventofcode2020

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1.txt").getLines.toList.map(_.toInt)

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[Int]): Int =
    lines
      .sliding(2)
      .count({ case List(a, b) => b > a })

  def part2(lines: List[Int]): Int =
    lines
      .sliding(3)
      .map(l => l.sum)
      .sliding(2)
      .count({ case Seq(a, b) => b > a })

}
