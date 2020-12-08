import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}
import scala.math.max

def makeTV(file: String): TreeMap[Int, Int] = {
  val bufferedSource: BufferedSource = Source.fromFile(file)
  val stringArray: Array[Int] = {
    bufferedSource
      .mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  }
  val unSortedMap: Map[Int, Int] = Iterator.from(0).zip(stringArray).toMap
  TreeMap[Int, Int]() ++ unSortedMap
}

// (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]

case class IntCode(memory: TreeMap[Int, Int], pointer: Int)

//def gasPlus(m: Int): Int = {
//  @tailrec
//  def gasAccumulator(m: Int, accum: Int): Int = {
//    val newGas: Int = max((m / 3) - 2, 0)
//    if (newGas > 0) {
//      gasAccumulator(newGas, accum + newGas)
//    } else {
//      accum
//    }
//  }
//
//  gasAccumulator(m, 0)
//}
