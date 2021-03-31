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

case class IntCode(pointer: Int, memory: Vector[Int])

object IntCode {
  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      val location1: Int = intCode.memory(intCode.pointer + 1)
      val location2: Int = intCode.memory(intCode.pointer + 2)
      val location3: Int = intCode.memory(intCode.pointer + 3)
      intCode.memory(intCode.pointer) match {
        case 1 =>
          val added: Int = intCode.memory(location1) + intCode.memory(location2)
          val newMemory: Vector[Int] = intCode.memory.updated(location3, added)
          recur(IntCode(pointer = intCode.pointer + 4, memory = newMemory))
        case 2 =>
          val multiplied: Int = intCode.memory(location1) * intCode.memory(location2)
          val newMemory: Vector[Int] = intCode.memory.updated(location3, multiplied)
          recur(IntCode(pointer = intCode.pointer + 4, memory = newMemory))
        case _ => intCode
      }
    }

    recur(intCode)
  }
}
