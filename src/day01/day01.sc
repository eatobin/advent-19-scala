import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.math.max

val bufferedSource: BufferedSource = Source.fromFile("day01.txt")
val gasList: Seq[Int] = bufferedSource.getLines.toSeq.map((s: String) => s.toInt)
bufferedSource.close

// part a
def gas(m: Int): Int = (m / 3) - 2

val answer: Int = gasList.map((m: Int) => gas(m)).sum

println(s"answer: $answer")

// 3337766

// part b
def gasPlus(m: Int): Int = {
  @tailrec
  def gasAccumulator(m: Int, accum: Int): Int = {
    val newGas: Int = max((m / 3) - 2, 0)
    if (newGas > 0) {
      gasAccumulator(newGas, accum + newGas)
    } else {
      accum
    }
  }

  gasAccumulator(m, 0)
}

val answer2: Int = gasList.map((m: Int) => gasPlus(m)).sum

println(s"answer2: $answer2")

// 5003788
