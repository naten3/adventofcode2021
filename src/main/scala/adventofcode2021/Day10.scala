package adventofcode2021

import scala.collection.mutable
import scala.io.Source

object Day10 {
  def main(args: Array[String]): Unit = {
    val lines =
      Source.fromResource("day10.txt").getLines.toList.map(_.toCharArray)

    println(part1(lines))
    println(part2(lines))
  }

  val openingByClosing = Map(']' -> '[', ')' -> '(', '}' -> '{', '>' -> '<')
  val scoreByClosing = Map(']' -> 57, ')' -> 3, '}' -> 1197, '>' -> 25137)
  val scoreByOpeningLeftOnStack = Map('[' -> 2, '(' -> 1, '{' -> 3, '<' -> 4)

  def getLineScore(line: Array[Char]): Int = {
    val stack = mutable.Stack[Char]()
    for (c <- line) {
      if (openingByClosing.contains(c)) {
        // closing char
        if (stack.top == openingByClosing(c)) {
          stack.pop()
        } else {
          return scoreByClosing(c)
        }
      } else {
        // opening char
        stack.push(c)
      }
    }
    0
  }

  def part1(lines: List[Array[Char]]): Int =
    lines
      .map(getLineScore)
      .sum

  def getLineScorePart2(line: Array[Char]): Long = {
    val stack = mutable.Stack[Char]()
    for (c <- line) {
      if (openingByClosing.contains(c)) {
        // closing char
        if (stack.top == openingByClosing(c)) {
          stack.pop()
        } else {
          // invalid stack
          return 0
        }
      } else {
        // opening char
        stack.push(c)
      }
    }
    stack.foldLeft(0L)((acc, next) => acc * 5 + scoreByOpeningLeftOnStack(next))
  }

  def part2(lines: List[Array[Char]]): Long = {
    val sortedScores =
      lines
        .map(getLineScorePart2)
        .filter(_ > 0)
        .sorted
        .toArray

    sortedScores(sortedScores.length / 2)
  }
}
