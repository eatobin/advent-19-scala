import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

def makeMemory(file: String): Vector[Int] = {
  val bufferedSource: BufferedSource = Source.fromFile(file)
  bufferedSource
    .mkString
    .split(",")
    .map(_.trim)
    .map(_.toInt)
    .toVector
}

// (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]
// a b or c = right-to-left position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

case class IntCode(cursor: Int, memory: Vector[Int])

object IntCode {
  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      val location1: Int = intCode.memory(intCode.cursor + 1)
      val location2: Int = intCode.memory(intCode.cursor + 2)
      val aPw: Int = intCode.memory(intCode.cursor + 3)
      intCode.memory(intCode.cursor) match {
        case 1 =>
          val added: Int = intCode.memory(location1) + intCode.memory(location2)
          val newMemory: Vector[Int] = intCode.memory.updated(aPw, added)
          recur(IntCode(cursor = intCode.cursor + 4, memory = newMemory))
        case 2 =>
          val multiplied: Int = intCode.memory(location1) * intCode.memory(location2)
          val newMemory: Vector[Int] = intCode.memory.updated(aPw, multiplied)
          recur(IntCode(cursor = intCode.cursor + 4, memory = newMemory))
        case _ => intCode
      }
    }

    recur(intCode)
  }
}
