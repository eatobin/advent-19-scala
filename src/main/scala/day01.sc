//$ amm --predef src/main/scala/day01.sc

import scala.io.Source

val bufferedSource = Source.fromFile("/home/eric/scala-projects/advent-19-scala/src/main/resources/day01.txt")
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
