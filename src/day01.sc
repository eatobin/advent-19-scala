import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

val bufferedSource: BufferedSource = Source.fromFile("resources/day01.txt")
val gasList: Seq[Int] = bufferedSource.getLines.toSeq.map((s: String) => s.toInt)
bufferedSource.close

// part a
def gas(m: Int): Int = (m / 3) - 2

val answer: Int = gasList.map((m: Int) => gas(m)).sum

println(s"Answer Part A: $answer")

// 3337766

// part b
def gasPlus(m: Int): Int = {
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

val answer2: Int = gasList.map((m: Int) => gasPlus(m)).sum

println(s"Answer Part B: $answer2")

// 5003788
