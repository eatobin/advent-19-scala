//$ amm --predef day01.sc
//$ scala-cli clean day01.sc
//$ scala-cli day01.sc
//$ scala-cli -S 2.13.8 day01.sc
//$ rm -rf .bsp/
//$ scala-cli repl -S 2.13.8
//$ scala-cli repl
//$ scala-cli repl --amm

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