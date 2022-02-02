package adventofcode2021

import scala.io.Source

object Day13 {
  val dotRegex = raw"([0-9]+),([0-9]+)".r
  val foldRegex = raw"fold along ([xy]+)=([0-9]+)".r

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13.txt").getLines.toList

    println(part1(lines))
    println(part2(lines))
  }

  def fold(set: Set[(Int, Int)], axis: String, value: Int): Set[(Int, Int)] = {
    if (axis == "y") {
      set.map({
        case (x, y) => {
          if (y <= value) {
            (x, y)
          } else {
            val newY = y - (y - value) * 2
            (x, newY)
          }
        }
      })
    } else {
      set.map({
        case (x, y) => {
          if (x <= value) {
            (x, y)
          } else {
            val newX = x - (x - value) * 2
            (newX, y)
          }
        }
      })
    }
  }

  def part1(lines: List[String]): Int = {
    val folded = lines
      .foldLeft((Set[(Int, Int)](), false))((acc, next) => {
        val (set, hasFolded) = acc
        next match {
          case dotRegex(x, y) => (set + ((x.toInt, y.toInt)), hasFolded)
          case foldRegex(axis, value) =>
            if (hasFolded) {
              acc
            } else {
              (fold(set, axis, value.toInt), true)
            }
          case _ => acc
        }
      })

    folded._1.size
  }

  def part2(lines: List[String]) = {
    val folded = lines
      .foldLeft(Set[(Int, Int)]())((set, next) => {
        next match {
          case dotRegex(x, y)         => set + ((x.toInt, y.toInt))
          case foldRegex(axis, value) => fold(set, axis, value.toInt)
          case _                      => set
        }
      })
    val (minX, maxX, minY, maxY) = folded.foldLeft(
      (
        Integer.MAX_VALUE,
        Integer.MIN_VALUE,
        Integer.MAX_VALUE,
        Integer.MIN_VALUE
      )
    )(
      (acc, next) =>
        (
          Math.min(acc._1, next._1),
          Math.max(acc._2, next._1),
          Math.min(acc._3, next._2),
          Math.max(acc._4, next._2)
      )
    )

    (minY to maxY).foreach(y => {
      val line = (minX to maxX)
        .map(x => if (folded.contains((x, y))) "#" else ".")
        .mkString
      System.out.println(line)
    })
  }

}
