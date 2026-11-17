// [eric@linux-epth day01]$ scala-cli test Day01ScalaTest.test.scala

//> using test.dep org.scalatest::scalatest-flatspec::3.2.20
//> using file Day01Scala.scala

import org.scalatest.flatspec.AnyFlatSpec

class Day01ScalaTest extends AnyFlatSpec {
  val sample: List[Int] = List(100756)

  "Part 1" should "handle sample input correctly" in {
    assert(Day01Scala.part1(sample) == 33_583)
  }

  "Part 2" should "handle sample input correctly" in {
    assert(Day01Scala.part2(sample) == 50_346)
  }
}
