package adventofcode2021

import scala.collection.mutable
import scala.io.Source

object Day14 {
  val ruleRegex = raw"([A-Z]+) -> ([A-Z])".r
  case class Node(value: Char, var next: Option[Node])
  case class Input(node: Node, rules: Map[(Char, Char), Char], line: String)

  def getInput(): Input = {
    val lines = Source.fromResource("day14.txt").getLines

    val compound = lines.next()
    val node = Node(compound.charAt(0), None)
    var currentNode = node
    (1 until compound.length).foreach(i => {
      currentNode.next = Some(Node(compound.charAt(i), None))
      currentNode = currentNode.next.get
    })

    val map = lines.foldLeft(Map[(Char, Char), Char]())((acc, next) => {
      next match {
        case ruleRegex(left, right) =>
          acc + (((left.charAt(0), left.charAt(1)) -> right.charAt(0)))
        case _ => acc
      }
    })
    Input(node, map, compound)
  }
  def main(args: Array[String]): Unit = {

    println(part1(getInput()))
    println(part2(getInput()))
  }

  def iterateNode(input: Input) = {
    val map = input.rules
    var currentNode = input.node
    while (currentNode.next.isDefined) {
      val ruleOpt = map.get(currentNode.value, currentNode.next.get.value)
      if (ruleOpt.isDefined) {
        val newNode = Node(ruleOpt.get, currentNode.next)
        currentNode.next = Some(newNode)
        currentNode = currentNode.next.get
      }
      currentNode = currentNode.next.get
    }
  }

  def makeList(nodeOpt: Option[Node]): List[Char] = {
    if (nodeOpt.isEmpty) {
      List()
    } else {
      nodeOpt.get.value +: makeList(nodeOpt.get.next)
    }
  }

  def part1(input: Input): Int = {
    (1 to 10).foreach((_) => (iterateNode(input)))
    val counts = mutable.Map[Char, Int]()
    var currentNode: Option[Node] = Some(input.node)
    while (currentNode.isDefined) {
      counts.put(
        currentNode.get.value,
        counts.getOrElse(currentNode.get.value, 0) + 1
      )
      currentNode = currentNode.get.next
    }
    val extrema = counts.foldLeft(
      ((' ', Integer.MAX_VALUE), (' ', Integer.MIN_VALUE))
    )((acc, next) => {
      val min = List(acc._1, next).minBy(_._2)
      val max = List(acc._2, next).maxBy(_._2)
      (min, max)
    })
    extrema._2._2 - extrema._1._2
  }

  def efficientIterate(
    values: List[((Char, Char), Long)],
    rulesMap: Map[(Char, Char), Char]
  ): List[((Char, Char), Long)] = {

    values
      .flatMap({
        case (tuple, count) => {
          if (rulesMap.contains(tuple)) {
            val newChar = rulesMap(tuple)
            List(((tuple._1, newChar), count), ((newChar, tuple._2), count))
          } else {
            List((tuple, count))
          }
        }
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
      .toList

  }

  def part2(input: Input): Long = {
    val pairs: List[(Char, Char)] =
      input.line.toCharArray.sliding(2).map(ar => (ar(0), ar(1))).toList
    val pairCounts: List[((Char, Char), Long)] =
      pairs.groupBy(a => a).mapValues(_.size.toLong).toList
    val result =
      (1 to 40).foldLeft(pairCounts)(
        (acc, _) => efficientIterate(acc, input.rules)
      )

    val charCounts: List[(Char, Long)] = result
      .flatMap({
        case ((charA, charB), count) => List((charA, count), (charB, count))
      })
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
      .toList

    val endLetters =
      List(input.line.charAt(0), input.line.last)
        .groupBy(a => a)
        .mapValues(_.size)
    val actualCharCounts = charCounts.map({
      case (c, count) => {
        val endCount = endLetters.getOrElse(c, 0)
        // the end letters are the only ones not shared. Take them off before dividing by two, then put them back
        (c, (count - endCount) / 2 + endCount)
      }
    })

    val min = actualCharCounts.minBy(_._2)._2
    val max = actualCharCounts.maxBy(_._2)._2

    max - min
  }

}
