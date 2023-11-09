// [eric@linux-epth day01]$ scala-cli Day01.sc
// Yay! Git works here!!

import scala.annotation.tailrec

private def fuel(mass: Int): Int = (mass / 3) - 2

private def fuelPlusFuel(mass: Int): Int =
  @tailrec
  def fuelAccumulator(m: Int, accum: Int): Int =
    val newGas: Int = fuel(m)
    if newGas <= 0
    then accum
    else fuelAccumulator(m = newGas, accum = accum + newGas)
  fuelAccumulator(m = mass, accum = 0)

def part1(input: Seq[Int]): Int = input.map(fuel).sum

def part2(input: Seq[Int]): Int = input.map(fuelPlusFuel).sum

val data = io.Source
  .fromFile("Day01.txt")
  .getLines()
  .map(_.toInt)
  .toSeq

println(part1(data)) // 3337766
println(part2(data)) // 5003788
