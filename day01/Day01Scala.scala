// [eric@linux-epth day01]$ scala-cli run Day01Scala.scala

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.io.Source

object Day01Scala:
  def fuel(mass: Int): Int = (mass / 3) - 2

  def fuelPlusFuel(mass: Int): Int =
    @tailrec
    def loop(m: Int, accum: Int): Int =
      val newGas: Int = fuel(m)
      if newGas <= 0
      then accum
      else loop(m = newGas, accum = accum + newGas)
      end if
    end loop
    loop(m = mass, accum = 0)
  end fuelPlusFuel

  def part1(input: List[Int]): Int = input.map(fuel).sum

  def part2(input: List[Int]): Int = input.map(fuelPlusFuel).sum

  @main
  def main(): Unit =
    val bufferedSource: BufferedSource = scala.io.Source.fromFile("Day01.txt")
    val data: List[Int] = bufferedSource
      .getLines()
      .map(_.toInt)
      .toList
    bufferedSource.close()

    println(part1(data)) // 3337766
    println(part2(data)) // 5003788
  end main
end Day01Scala
