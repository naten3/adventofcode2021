package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day3.txt").getLines.toList

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[String]): Int = {
    val oneCount: Array[Int] = Array.ofDim(lines(0).length);
    lines.foreach(
      l =>
        l.toCharArray.zipWithIndex
          .filter({ case (char, _) => char == '0' })
          .foreach({
            case (_, index) => oneCount.update(index, oneCount(index) + 1)
          })
    )

    val halfLength = lines.length / 2F;
    val gammaRateBin =
      oneCount.map(count => if (count > halfLength) '1' else '0').mkString
    val deltaRateBin =
      gammaRateBin.map(v => if (v == '1') '0' else '1').mkString

    Seq(gammaRateBin, deltaRateBin).map(Integer.parseInt(_, 2)).product
  }

  @tailrec
  def reduceToFinalLine(lines: Seq[String],
                        bitPosition: Int,
                        mostCommon: Boolean): String = {
    if (lines.length == 1) {
      return lines(0)
    }

    val (oneLines, zeroLines) = lines.partition(_.charAt(bitPosition) == '1')
    val keepers =
      if (mostCommon) {
        if (zeroLines.length > oneLines.length) zeroLines else oneLines
      } else {
        if (oneLines.length < zeroLines.length) oneLines else zeroLines
      }
    reduceToFinalLine(keepers, bitPosition + 1, mostCommon)
  }

  def part2(lines: List[String]): Int = {
    val oxygenRating = reduceToFinalLine(lines, 0, true)
    val co2Rating = reduceToFinalLine(lines, 0, false)
    Seq(oxygenRating, co2Rating).map(Integer.parseInt(_, 2)).product
  }
}
