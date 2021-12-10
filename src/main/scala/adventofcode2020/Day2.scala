package adventofcode2020

import scala.io.Source

object Day2 {
  val inputRegex = raw"^([a-z]*) ([0-9]*)$$".r

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2.txt").getLines.toList

    println(part1(lines))
    println(part2(lines))
  }

  def getNewCoord(dir: String, dist: Int, coord: (Int, Int)) = {
    dir match {
      case "forward" => (coord._1 + dist, coord._2);
      case "up"      => (coord._1, coord._2 + dist);
      case "down"    => (coord._1, coord._2 - dist);
    }
  }

  def part1(lines: List[String]): Int = {
    val finalPosition: (Int, Int) = lines.foldLeft((0, 0))(
      (position: (Int, Int), line: String) =>
        line match {
          case inputRegex(dir, dist) => getNewCoord(dir, dist.toInt, position)
      }
    )

    Math.abs(finalPosition._1 * finalPosition._2)
  }

  def getNewCoordWithAim(dir: String,
                         dist: Int,
                         coordWithAim: (Int, Int, Int)) = {
    dir match {
      case "forward" =>
        (
          coordWithAim._1 + dist,
          Math.max(0, coordWithAim._2 + coordWithAim._3 * dist),
          coordWithAim._3
        );
      case "up"   => (coordWithAim._1, coordWithAim._2, coordWithAim._3 - dist);
      case "down" => (coordWithAim._1, coordWithAim._2, coordWithAim._3 + dist);
    }
  }

  def part2(lines: List[String]): Int = {
    val finalPosition: (Int, Int, Int) = lines.foldLeft((0, 0, 0))(
      (position: (Int, Int, Int), line: String) =>
        line match {
          case inputRegex(dir, dist) =>
            getNewCoordWithAim(dir, dist.toInt, position)
      }
    )

    Math.abs(finalPosition._1 * finalPosition._2)
  }

}
