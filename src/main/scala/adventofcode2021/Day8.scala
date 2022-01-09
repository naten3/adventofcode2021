package adventofcode2021

import scala.io.Source

case class InputLine(inputs: List[String], outputs: List[String])

object Day8 {
  val inputRegex = raw"^(.*) \| (.*)$$".r

  def parseInput(): Array[InputLine] = {
    Source
      .fromResource("day8.txt")
      .getLines
      .map({
        case inputRegex(inputs, outputs) =>
          InputLine(
            inputs.trim.split(' ').toList,
            outputs.trim.split(' ').toList
          )
      })
      .toArray
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput()
    println(part1(input))
    println(part2(input))
  }

  val easySizes = Set(2, 4, 3, 7)
  def part1(lines: Array[InputLine]): Int =
    lines.map(_.outputs.count(s => easySizes.contains(s.length))).sum

  val digitPatterns = Map(
    0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
    1 -> Set('c', 'f'),
    2 -> Set('a', 'c', 'd', 'e', 'g'),
    3 -> Set('a', 'c', 'd', 'f', 'g'),
    4 -> Set('b', 'c', 'd', 'f'),
    5 -> Set('a', 'b', 'd', 'f', 'g'),
    6 -> Set('a', 'b', 'd', 'e', 'f', 'g'),
    7 -> Set('a', 'c', 'f'),
    8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    9 -> Set('a', 'b', 'c', 'd', 'f', 'g')
  )

  val digitByPattern: Map[Set[Char], Int] = for ((k, v) <- digitPatterns)
    yield (v, k)

  val easyDigits = Map(2 -> 1, 4 -> 4, 3 -> 7, 7 -> 8)

  def decodeFilteredSet(potentialMappings: Map[Char, Set[Char]],
                        inputs: List[String]): Option[Map[Char, Char]] = {
    if (potentialMappings.find(_._2.isEmpty).isDefined) {
      return None
    }
    if (potentialMappings.forall(_._2.size == 1)) {
      // base case, see if it's valid
      val mappings = potentialMappings.view.mapValues(s => s.head).toMap
      Some(mappings).filter(isValid(_, inputs))
    } else {
      // too lazy to validate intermediate states, hope there aren't too many combinations left
      val fixedItem =
        potentialMappings.find(_._2.size > 1).get
      val lockedChar = fixedItem._1
      // choose a mapping for this item and see it if yields a valid mapping
      val result = fixedItem._2
        .map(char => {
          // see if this char is the right mapping
          val newMap = potentialMappings.transform(
            (c: Char, set: Set[Char]) =>
              if (c == lockedChar) {
                Set(char)
              } else {
                set - char
            }
          )
          decodeFilteredSet(newMap, inputs)
        })
        .find(_.isDefined)
        .flatten
      result
    }
  }

  def isValid(mapping: Map[Char, Char], inputs: List[String]): Boolean = {
    // does every input map to a valid digit
    inputs.forall(input => {
      val mappedInput = input.map(c => mapping(c)).toSet
      digitByPattern.contains(mappedInput)
    })
  }

  def decodeSet(line: InputLine): Int = {
    val allChars = Set('a', 'b', 'c', 'd', 'e', 'f', 'g')
    val potentialMappings =
      allChars
        .map(
          c =>
            c -> {
              val set =
                scala.collection.mutable.Set[Char]()
              set.addAll(allChars)
              set
          }
        )
        .toMap

    // first narrrow down potential mappings with easy digits
    line.inputs
      .filter(s => easyDigits.contains(s.length))
      .foreach(s => {
        val charSet = s.toSet
        val digit = easyDigits.get(s.length).get
        val digitPattern = digitPatterns.get(digit).get
        val complementSet = allChars.removedAll(digitPattern)
        potentialMappings.foreachEntry((c, potentialMapping) => {
          // the chars in this number must map to the real ones, and nothing else can map to them
          val excludeSet = if (charSet.contains(c)) {
            complementSet
          } else { digitPattern }

          excludeSet.foreach(potentialMapping.remove(_))
        })
      })

    // now guess
    val resultMap = decodeFilteredSet(
      potentialMappings.mapValues(_.toSet).toMap,
      line.inputs
    ).get
    line.outputs
      .map(output => {
        val set = output.map(resultMap(_)).toSet
        val digit: Int = digitByPattern(set)
        digit
      })
      .mkString
      .toInt
  }

  def part2(lines: Array[InputLine]): Int = lines.map(decodeSet).sum

}
