package adventofcode2021

import adventofcode2021.Day4.PuzzleInput

import scala.collection.mutable
import scala.io.Source

object Day12 {

  val inputRegex = raw"([a-zA-Z]+)-([a-zA-Z]+)".r
  type PuzzleInput = mutable.Map[String, mutable.Set[String]];
  val START = "start"
  val END = "end"
  def main(args: Array[String]): Unit = {
    val lines: PuzzleInput = mutable.Map()

    def addItem(s1: String, s2: String) = {
      val set = {
        if (lines.contains(s1)) lines(s1)
        else {
          val s = mutable.Set[String]()
          lines.put(s1, s)
          s
        }
      }
      set.add(s2)
    }

    Source
      .fromResource("day12.txt")
      .getLines
      .foreach(_ match {
        case inputRegex(s1, s2) => {
          if (s2 != START)
            addItem(s1, s2)
          if (s1 != START)
            addItem(s2, s1)
        }
      })

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: PuzzleInput): Int = {
    def findPaths(node: String, visitedSmall: Set[String]): Int = {
      if (node == END) {
        return 1
      }
      val nodeChar = node.toCharArray()(0)
      val isSmallCave = node != START && nodeChar >= 'a' && nodeChar <= 'z'
      val newVisited = if (isSmallCave) visitedSmall + node else visitedSmall

      var sum = 0
      lines
        .getOrElse(node, Set())
        .filter(!visitedSmall.contains(_))
        .foreach(neighbor => {
          val r = findPaths(neighbor, newVisited)
          sum = sum + r
        })

      sum
    }

    findPaths(START, Set())
  }

  def part2(lines: PuzzleInput): Int = {
    def findPaths(node: String,
                  visitedSmall: Set[String],
                  twiceVisited: Option[String]): Int = {
      if (node == END) {
        return 1
      }
      val nodeChar = node.toCharArray()(0)
      val isSmallCave = node != START && nodeChar >= 'a' && nodeChar <= 'z'
      val newVisited =
        if (isSmallCave) {
          val newVal = if (visitedSmall.contains(node)) 2 else 1
          visitedSmall + node
        } else visitedSmall

      var sum = 0
      lines
        .getOrElse(node, Set())
        .foreach(neighbor => {
          if (!visitedSmall.contains(neighbor)) {
            sum = sum + findPaths(neighbor, newVisited, twiceVisited)
          } else if (!twiceVisited.isDefined) {
            sum = sum + findPaths(neighbor, newVisited, Some(neighbor))
          }
        })

      sum
    }

    findPaths(START, Set(), None)
  }

}
