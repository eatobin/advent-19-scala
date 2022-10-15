//$ amm --predef day01.sc

import scala.annotation.tailrec
import scala.io.Source

val bufferedSource = Source.fromFile("day01.txt")
val gasList = bufferedSource.getLines().toList.map(s => s.toInt)
bufferedSource.close

// part a
def gas(m: Int): Int = (m / 3) - 2

val answer: Int = gasList.map(m => gas(m)).sum

println(s"Answer Part A: $answer")

// 3337766

// part b
def gasPlus(m: Int): Int = {
  (LazyList.iterate(m) { x => gas(x) } takeWhile (y => y >= 0)).tail.sum
}

val answer2: Int = gasList.map(m => gasPlus(m)).sum

println(s"Answer Part B: $answer2")

// 5003788

// part c (really b)
def gasPlus2(m: Int): Int = {
  @tailrec
  def gasAccumulator(m: Int, accum: Int): Int = {
    val newGas: Int = gas(m)
    if (newGas > 0) {
      gasAccumulator(m = newGas, accum = accum + newGas)
    } else {
      accum
    }
  }

  gasAccumulator(m = m, accum = 0)
}

val answer3: Int = gasList.map((m: Int) => gasPlus2(m)).sum

println(s"Answer Part C: $answer3")

// 5003788
