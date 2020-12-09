import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}

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

case class IntCode(pointer: Int, memory: TreeMap[Int, Int])

object IntCode {
  def opCode(intCode: IntCode): IntCode = intCode.memory(intCode.pointer) match {
    case 1 =>
      val added: Int = intCode.memory(intCode.pointer + 1) + intCode.memory(intCode.pointer + 2)
      val newMemory: TreeMap[Int, Int] = intCode.memory + (intCode.memory(intCode.pointer + 3) -> added)
      IntCode(intCode.pointer + 4, newMemory)
  }
}

val testing: IntCode = IntCode(0, TreeMap(0 -> 1, 1 -> 99, 2 -> 1, 3 -> 4))
println(IntCode.opCode(testing))

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
