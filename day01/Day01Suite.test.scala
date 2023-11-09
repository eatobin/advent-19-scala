// [eric@linux-epth day01]$ scala-cli test Day01Suite.test.scala

//> using test.dep org.scalatest::scalatest::3.2.17
//> using file Day01.scala

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample: Seq[Int] = Seq(100756)

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample) == 33_583)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample) == 50_346)
  }
