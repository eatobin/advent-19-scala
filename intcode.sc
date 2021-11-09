//$ amm --predef intcode.sc

//import scala.annotation.tailrec

import scala.collection.immutable.{ListMap, TreeMap}
import scala.io.{BufferedSource, Source}

type FilePath = String
type Memory = TreeMap[Int, Int]
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
  val unSortedMap: Map[Int, Int] = Iterator.from(0).zip(stringArray).toMap
  TreeMap[Int, Int]() ++ unSortedMap
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

  /*def aPw(intCode: IntCode): Int = intCode.memory(intCode.pointer + 3)

  def bPrbRr(intCode: IntCode): Int =
    intCode.memory.getOrElse(intCode.memory(intCode.pointer + 2) + intCode.relativeBase, 0)

  def cPrcRr(intCode: IntCode): Int =
    intCode.memory.getOrElse(intCode.memory(intCode.pointer + 1) + intCode.relativeBase, 0)

  def cPwcIr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 1)

  def bIr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 2)

  def aRw(intCode: IntCode): Int = intCode.memory(intCode.pointer + 3) + intCode.relativeBase

  def cRw(intCode: IntCode): Int = intCode.memory(intCode.pointer + 1) + intCode.relativeBase

  def addressMakerC(intCode: IntCode): Int = {
    pad5(intCode.memory(intCode.pointer))('e') match {
      case 1 | 2 | 4 | 5 | 6 | 7 | 8 | 9 =>
        pad5(intCode.memory(intCode.pointer))('c') match {
          case 0 | 2 => cPrcRr(intCode)
          case 1 => cPwcIr(intCode)
        }
      case 3 =>
        pad5(intCode.memory(intCode.pointer))('c') match {
          case 0 => cPwcIr(intCode)
          case 2 => cRw(intCode)
        }
    }
  }

  def addressMakerB(intCode: IntCode): Int = {
    pad5(intCode.memory(intCode.pointer))('b') match {
      case 0 | 2 => bPrbRr(intCode)
      case 1 => bIr(intCode)
    }
  }

  def addressMakerA(intCode: IntCode): Int = {
    pad5(intCode.memory(intCode.pointer))('a') match {
      case 0 => aPw(intCode)
      case 2 => aRw(intCode)
    }
  }

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      if (intCode.stopped) {
        intCode
      } else {
        pad5(intCode.memory(intCode.pointer))('e') match {
          case 9 =>
            if (pad5(intCode.memory(intCode.pointer))('d') == 9)
              intCode.copy(stopped = true) else {
              recur(IntCode(
                input = intCode.input,
                output = intCode.output,
                phase = intCode.phase,
                pointer = intCode.pointer + 2,
                relativeBase = addressMakerC(intCode) + intCode.relativeBase,
                memory = intCode.memory,
                stopped = intCode.stopped,
                recur = intCode.recur))
            }
          case 1 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = intCode.memory.updated(addressMakerA(intCode), addressMakerC(intCode) + addressMakerB(intCode)),
              stopped = intCode.stopped,
              recur = intCode.recur))
          case 2 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = intCode.memory.updated(addressMakerA(intCode), addressMakerC(intCode) * addressMakerB(intCode)),
              stopped = intCode.stopped,
              recur = intCode.recur))
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
                    intCode.memory.updated(addressMakerC(intCode), intCode.phase)
                  } else {
                    intCode.memory.updated(addressMakerC(intCode), intCode.input)
                  }
                } else {
                  intCode.memory.updated(addressMakerC(intCode), intCode.input)
                },
              stopped = intCode.stopped,
              recur = intCode.recur))
          case 4 =>
            if (intCode.recur) {
              recur(IntCode(
                input = intCode.input,
                output = addressMakerC(intCode),
                phase = intCode.phase,
                pointer = intCode.pointer + 2,
                relativeBase = intCode.relativeBase,
                memory = intCode.memory,
                stopped = intCode.stopped,
                recur = intCode.recur))
            } else {
              IntCode(
                input = intCode.input,
                output = addressMakerC(intCode),
                phase = intCode.phase,
                pointer = intCode.pointer + 2,
                relativeBase = intCode.relativeBase,
                memory = intCode.memory,
                stopped = intCode.stopped,
                recur = intCode.recur)
            }
          case 5 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = if (addressMakerC(intCode) == 0) {
                intCode.pointer + 3
              } else {
                addressMakerB(intCode)
              },
              relativeBase = intCode.relativeBase,
              memory = intCode.memory,
              stopped = intCode.stopped,
              recur = intCode.recur))
          case 6 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = if (addressMakerC(intCode) != 0) {
                intCode.pointer + 3
              } else {
                addressMakerB(intCode)
              },
              relativeBase = intCode.relativeBase,
              memory = intCode.memory,
              stopped = intCode.stopped,
              recur = intCode.recur))
          case 7 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = if (addressMakerC(intCode) < addressMakerB(intCode)) {
                intCode.memory.updated(addressMakerA(intCode), 1)
              } else {
                intCode.memory.updated(addressMakerA(intCode), 0)
              },
              stopped = intCode.stopped,
              recur = intCode.recur))
          case 8 =>
            recur(IntCode(
              input = intCode.input,
              output = intCode.output,
              phase = intCode.phase,
              pointer = intCode.pointer + 4,
              relativeBase = intCode.relativeBase,
              memory = if (addressMakerC(intCode) == addressMakerB(intCode)) {
                intCode.memory.updated(addressMakerA(intCode), 1)
              } else {
                intCode.memory.updated(addressMakerA(intCode), 0)
              },
              stopped = intCode.stopped,
              recur = intCode.recur))
        }
      }
    }

    recur(intCode)
  }*/
}
