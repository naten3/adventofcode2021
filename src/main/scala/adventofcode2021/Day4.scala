package adventofcode2021

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day4 {
  case class PuzzleInput(numbers: List[Int], boards: Seq[Seq[Seq[Int]]])

  def processInput(lines: Seq[String]): PuzzleInput = {
    val iterator = lines.iterator;
    val numbers = iterator.next.split(',').map(Integer.parseInt).toList
    iterator.next

    val boards = scala.collection.mutable.ListBuffer[Seq[Seq[Int]]]()

    var board = new scala.collection.mutable.ListBuffer[Seq[Int]];
    while (iterator.hasNext) {
      val next = iterator.next;
      if (next.isEmpty) {
        boards.append(board.toList)
        board = ListBuffer();
      } else {
        board.append(next.split(' ').filter(!_.isEmpty).map(Integer.parseInt))
      }
    }
    boards.append(board.toList)

    PuzzleInput(numbers, boards.toList)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4.txt").getLines.toSeq

    val input = processInput(lines);

    println(part1(input))
    println(part2(input))
  }

  case class BoardData(allBoardNumbers: Set[Int],
                       lines: Seq[Set[Int]],
                       hasBingo: Boolean)

  def callNumber(boardData: BoardData, number: Int): BoardData = {
    val allNumbers = boardData.allBoardNumbers - number
    val (newLines, hasBingo) =
      boardData.lines.foldLeft(List[Set[Int]](), boardData.hasBingo)(
        (linesWithBingo, line) => {
          val newLine = line - number
          val bingo = linesWithBingo._2 || newLine.isEmpty
          (linesWithBingo._1 :+ newLine, bingo)
        }
      )
    BoardData(allNumbers, newLines, hasBingo)
  }

  // run the numbers until bingo condition is met
  def callNumbers(boardData: Seq[BoardData], numbers: List[Int]): Int = {
    val nextNum = numbers.head;
    val newBoardData = scala.collection.mutable.ListBuffer[BoardData]()

    boardData.foreach(b => {
      val newBoard = callNumber(b, nextNum);
      if (newBoard.hasBingo) {
        return newBoard.allBoardNumbers.sum * nextNum
      }
      newBoardData.append(newBoard)
    })

    callNumbers(newBoardData.toList, numbers.tail)
  }

  def generateBoardData(input: PuzzleInput) = {
    input.boards
      .map(b => {
        val allNumbers = b.flatten.toSet
        val lines = b.map(_.toSet) ++ (0 until b(0).length)
          .map(index => b.map(_(index)).toSet)
        BoardData(allNumbers, lines, false)
      })
      .toArray
  }

  def part1(input: PuzzleInput): Int = {
    val boardData = generateBoardData(input)
    callNumbers(boardData, input.numbers)
  }

  // run the numbers until bingo condition is met for last board
  def callNumbersFindLoser(boardData: Seq[BoardData],
                           numbers: List[Int]): Int = {
    val nextNum = numbers.head
    val newBoards = boardData.map(callNumber(_, nextNum))
    if (newBoards.length == 1 && newBoards(0).hasBingo) {
      val loserBoard = newBoards(0)
      return loserBoard.allBoardNumbers.sum * nextNum
    }
    callNumbersFindLoser(newBoards.filter(!_.hasBingo), numbers.tail)
  }

  def part2(input: PuzzleInput): Int = {
    val boardData = generateBoardData(input)
    callNumbersFindLoser(boardData, input.numbers)
  }
}
