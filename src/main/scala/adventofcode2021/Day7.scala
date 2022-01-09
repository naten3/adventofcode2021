package adventofcode2021

import scala.collection.mutable
import scala.io.Source

object Day7 {
  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromResource("day7.txt")
      .getLines()
      .next
      .split(",")
      .map(_.toInt)

    println(part1(lines))
    println(part2(lines))
  }

  def partition(array: Array[Int],
                left: Int,
                right: Int,
                pivotIndex: Int): Int = {

    def swap(i: Int, j: Int) = {
      val temp = array(i)
      array(i) = array(j)
      array(j) = temp
    }

    val pivotValue = array(pivotIndex)
    var storeIndex = left
    (left until right).foreach(j => {
      if (array(j) < pivotValue) {
        swap(j, storeIndex)
        storeIndex = storeIndex + 1
      }
    })
    swap(right, storeIndex)

    storeIndex
  }

  def selectMedianIndex(list: Array[Int], left: Int, right: Int): Int = {
    val k = list.length / 2 - 1
    if (left == right) {
      return left
    }
    var pivotIndex = left
    pivotIndex = partition(list, left, right, pivotIndex)
    if (k == pivotIndex) {
      k
    } else if (k < pivotIndex) {
      selectMedianIndex(list, left, pivotIndex - 1)
    } else {
      selectMedianIndex(list, pivotIndex + 1, right)
    }
  }

  def part1(lines: Seq[Int]): Int = {
    // quickselect implementation
    val linesAr = lines.toArray
    val medianIndex = selectMedianIndex(linesAr, 0, lines.length - 1)

    val median = if (lines.length % 2 == 0) {
      linesAr(medianIndex)
    } else {
      Math.round((linesAr(medianIndex) + linesAr(medianIndex + 1)).toFloat / 2)
    }

    linesAr.map(v => Math.abs(v - median)).sum
  }

  def findDistanceFuel(start: Int, end: Int): Int = {
    val n = Math.abs(end - start)
    n * (n + 1) / 2
  }

  def totalFuelDist(lines: Seq[Int], point: Int) =
    lines.map(findDistanceFuel(_, point)).sum

  def part2(lines: Seq[Int]): Int = {
    var (start, end) = lines.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE))({
      case ((min, max), next) => (Math.min(min, next), Math.max(max, next))
    })

    var (startFuel, endFuel) =
      (totalFuelDist(lines, start), totalFuelDist(lines, end))

    while (end - start > 1) {
      val mid = (start + end) / 2
      val midFuel = totalFuelDist(lines, mid)
      if (endFuel < startFuel) {
        start = mid;
        startFuel = midFuel
      } else {
        end = mid;
        endFuel = midFuel
      }
    }
    Math.min(startFuel, endFuel)
  }

  /* lines.foldLeft(Integer.MAX_VALUE)((currentMin, nextVal) => {
      var totalFuel = 0
      lines
        .takeWhile((_) => totalFuel < currentMin)
        .foreach(f => {
          totalFuel += findDistanceFuel(f, nextVal)
        })

      if (totalFuel < currentMin) {
        totalFuel
      } else {
        currentMin
      }
    })
  }*/
}
