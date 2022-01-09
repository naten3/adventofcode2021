package adventofcode2021

import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromResource("day6.txt")
      .getLines()
      .next
      .split(",")
      .map(_.toInt)

    println(part1(lines))
    println(part2(lines))
  }

  def simulateLanterfishDay(fish: Seq[Int]): Seq[Int] =
    fish.flatMap(f => if (f == 0) Seq(6, 8) else Seq(f - 1))

  def part1(lines: Seq[Int]): Int =
    (1 to 80).foldLeft(lines)((fish, _) => simulateLanterfishDay(fish)).length

  def part2(lines: Seq[Int]): Long = {
    val resultMap = scala.collection.mutable.Map[Int, Long]()
    // how many fish produced by a new fish (including the fish itself) with d days remaining
    def fishOffspring(d: Int): Long = {
      if (resultMap.contains(d)) {
        return resultMap(d)
      }

      var totalFish = 1L
      var daysToFishBreed = 9
      var remainingDays = d
      while (remainingDays > daysToFishBreed) {
        val nextFishBreed = remainingDays - (daysToFishBreed)
        // simulate a new fish
        totalFish = totalFish + fishOffspring(nextFishBreed)
        remainingDays = nextFishBreed
        daysToFishBreed = 7
      }

      resultMap.put(d, totalFish)
      totalFish
    }

    val totalDays = 256
    // adjust the total days to simulate each fish being new
    lines.map(l => fishOffspring(totalDays + (8 - l + 1))).sum
  }
}
