// [eric@linux-epth day01]$ scala-cli Day01Sc.sc
// Yay! Git works here!!

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.io.Source

private def fuel(mass: Int): Int = (mass / 3) - 2

private def fuelPlusFuel(mass: Int): Int =
  @tailrec
  def loop(m: Int, accum: Int): Int =
    val newGas: Int = fuel(m)
    if newGas <= 0
    then accum
    else loop(m = newGas, accum = accum + newGas)

  loop(m = mass, accum = 0)

def part1(input: Seq[Int]): Int = input.map(fuel).sum

def part2(input: Seq[Int]): Int = input.map(fuelPlusFuel).sum

val bufferedSource: BufferedSource = scala.io.Source.fromFile("Day01.txt")

val data = bufferedSource
  .getLines()
  .map(_.toInt)
  .toSeq
bufferedSource.close()

println(part1(data)) // 3337766
println(part2(data)) // 5003788
