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

case class IntCode(output: Int, pointer: Int, memory: Vector[Int])

object IntCode {
  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      val cPr: Int = intCode.memory(intCode.memory(intCode.pointer + 1))
      val bPr: Int = intCode.memory(intCode.memory(intCode.pointer + 2))
      val aIw: Int = intCode.memory(intCode.pointer + 3)
      intCode.memory(intCode.pointer) match {
        case 1 =>
          val added: Int = cPr + bPr
          val newMemory: Vector[Int] = intCode.memory.updated(aIw, added)
          recur(IntCode(output = newMemory(0), pointer = intCode.pointer + 4, memory = newMemory))
        case 2 =>
          val multiplied: Int = cPr * bPr
          val newMemory: Vector[Int] = intCode.memory.updated(aIw, multiplied)
          recur(IntCode(output = newMemory(0), pointer = intCode.pointer + 4, memory = newMemory))
        case _ => intCode
      }
    }

    recur(intCode)
  }
}
