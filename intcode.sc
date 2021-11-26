//$ amm --predef intcode.sc

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, TreeMap}
import scala.io.{BufferedSource, Source}

type FilePath = String
type Memory = Map[Int, Int]
type Instruction = ListMap[Char, Int]

def makeMemory(file: FilePath): Memory = {
  val bufferedSource: BufferedSource = Source.fromFile(file)
  val stringArray: Array[Int] = {
    bufferedSource
      .mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  }
  Iterator.from(0).zip(stringArray).toMap
}

def makeShortMemory(coll: Array[Int]): Memory = {
  val unSortedMap: Map[Int, Int] = Iterator.from(0).zip(coll).toMap
  TreeMap[Int, Int]() ++ unSortedMap
}

def pad5(op: Int): Instruction = {
  val inBytes: Array[Int] = "%05d".format(op).getBytes.map(_ - 48)
  val inMap: Map[Char, Int] = Array('a', 'b', 'c', 'd', 'e').zip(inBytes).toMap
  ListMap(inMap.toSeq.sortBy(_._1): _*)
}

// (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]

// ABCDE
// 01002

// a b or c = left-to-right position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

case class IntCode(input: Int, output: Int, phase: Int, pointer: Int, relativeBase: Int, memory: Memory, isStopped: Boolean, doesRecur: Boolean)

object IntCode {
  private val offsetC: Int = 1
  private val offsetB: Int = 2
  private val offsetA: Int = 3

  def aParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('a') match {
      // a-p-w
      case 0 => intcode.memory(intcode.pointer + offsetA)
      // a-r-w
      case 2 => intcode.memory(intcode.pointer + offsetA) + intcode.relativeBase
    }
  }

  def bParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('b') match {
      // b-p-r
      case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetB), 0)
      // b-i-r
      case 1 => intcode.memory(intcode.pointer + offsetB)
      // b-r-r
      case 2 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetB) + intcode.relativeBase, 0)
    }
  }

  def cParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('e') match {
      case 3 =>
        instruction('c') match {
          // c-p-w
          case 0 => intcode.memory(intcode.pointer + offsetC)
          // c-r-w
          case 2 => intcode.memory(intcode.pointer + offsetC) + intcode.relativeBase
        }
      case _ =>
        instruction('c') match {
          // c-p-r
          case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC), 0)
          // c-i-r
          case 1 => intcode.memory(intcode.pointer + offsetC)
          // c-r-r
          case 2 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC) + intcode.relativeBase, 0)
        }
    }
  }

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      if (intCode.isStopped) {
        intCode
      } else {
        val instruction: Instruction = pad5(intCode.memory(intCode.pointer))
        instruction('e') match {
          case 9 =>
            if (instruction('d') == 9)
              intCode.copy(isStopped = true) else {
              recur(IntCode(
                input = intCode.input,
                output = intCode.output,
                phase = intCode.phase,
                pointer = intCode.pointer + 2,
                relativeBase = cParam(instruction, intCode) + intCode.relativeBase,
                memory = intCode.memory,
                isStopped = intCode.isStopped,
                doesRecur = intCode.doesRecur))
            }
          case 1 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) + bParam(instruction, intCode)),
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
          case 2 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) * bParam(instruction, intCode)),
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
          case 3 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 2,
              relativeBase = intCode.relativeBase,
              memory =
                if (intCode.phase >= 0 && intCode.phase <= 9) {
                  if (intCode.pointer == 0) {
                    intCode.memory.updated(cParam(instruction, intCode), intCode.phase)
                  } else {
                    intCode.memory.updated(cParam(instruction, intCode), intCode.input)
                  }
                } else {
                  intCode.memory.updated(cParam(instruction, intCode), intCode.input)
                },
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
          case 4 =>
            if (intCode.doesRecur) {
              recur(IntCode(
                input = intCode.input,
                output = cParam(instruction, intCode),
                phase = intCode.phase,
                pointer = intCode.pointer + 2,
                relativeBase = intCode.relativeBase,
                memory = intCode.memory,
                isStopped = intCode.isStopped,
                doesRecur = intCode.doesRecur))
            } else {
              IntCode(
                input = intCode.input,
                output = cParam(instruction, intCode),
                phase = intCode.phase,
                pointer = intCode.pointer + 2,
                relativeBase = intCode.relativeBase,
                memory = intCode.memory,
                isStopped = intCode.isStopped,
                doesRecur = intCode.doesRecur)
            }
          case 5 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = if (cParam(instruction, intCode) == 0) {
                intCode.pointer + 3
              } else {
                bParam(instruction, intCode)
              },
              relativeBase = intCode.relativeBase,
              memory = intCode.memory,
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
          case 6 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = if (cParam(instruction, intCode) != 0) {
                intCode.pointer + 3
              } else {
                bParam(instruction, intCode)
              },
              relativeBase = intCode.relativeBase,
              memory = intCode.memory,
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
          case 7 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = if (cParam(instruction, intCode) < bParam(instruction, intCode)) {
                intCode.memory.updated(aParam(instruction, intCode), 1)
              } else {
                intCode.memory.updated(aParam(instruction, intCode), 0)
              },
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
          case 8 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = if (cParam(instruction, intCode) == bParam(instruction, intCode)) {
                intCode.memory.updated(aParam(instruction, intCode), 1)
              } else {
                intCode.memory.updated(aParam(instruction, intCode), 0)
              },
              isStopped = intCode.isStopped,
              doesRecur = intCode.doesRecur))
        }
      }
    }

    recur(intCode)
  }
}
