import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}

def makeMemory(file: String): TreeMap[Int, Int] = {
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
  def ic(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = intCode.memory(intCode.pointer) match {
      case 1 =>
        val added: Int = intCode.memory(intCode.memory(intCode.pointer + 1)) + intCode.memory(intCode.memory(intCode.pointer + 2))
        val newMemory: TreeMap[Int, Int] = intCode.memory + (intCode.memory(intCode.pointer + 3) -> added)
        recur(IntCode(pointer = intCode.pointer + 4, memory = newMemory))
      case 2 =>
        val multiplied: Int = intCode.memory(intCode.memory(intCode.pointer + 1)) * intCode.memory(intCode.memory(intCode.pointer + 2))
        val newMemory: TreeMap[Int, Int] = intCode.memory + (intCode.memory(intCode.pointer + 3) -> multiplied)
        recur(IntCode(pointer = intCode.pointer + 4, memory = newMemory))
      case _ => intCode
    }

    recur(intCode)
  }
}
