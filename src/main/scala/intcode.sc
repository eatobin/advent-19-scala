//$ amm --predef intcode.sc

import scala.annotation.tailrec
import scala.io.Source

type FilePath = String
type Memory = Map[Int, Int]
type Instruction = Map[Char, Int]

def makeMemory(file: FilePath): Memory = {
  val bufferedSource = Source.fromFile(file)
  val intArray = {
    bufferedSource
      .mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  }
  bufferedSource.close
  Iterator.from(0).zip(intArray).toMap
}

def charToInt(aChar: Byte): Int = {
  if (aChar < 48 || aChar > 57)
    throw new Exception("Char is not an integer")
  else aChar - 48
}

def pad5(op: Int): Instruction = {
  val inInts = "%05d".format(op).getBytes.map(charToInt)
  Array('a', 'b', 'c', 'd', 'e').zip(inInts).toMap
}

// ABCDE
// 01002

// a b or c = left-to-right position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

final case class IntCode(input: Int, output: Int, phase: Int, pointer: Int, relativeBase: Int, memory: Memory, isStopped: Boolean, doesRecur: Boolean)

//final case class IntCode(pointer: Int, memory: Memory)

object IntCode {
  private val offsetC: Int = 1
  private val offsetB: Int = 2
  private val offsetA: Int = 3

  def aParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('a') match {
      case 0 => intcode.memory(intcode.pointer + offsetA) // a-p-w
    }
  }

  def bParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('b') match {
      case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetB), 0) // b-p-r
      case 1 => intcode.memory(intcode.pointer + offsetB) // b-i-r
    }
  }

  def cParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('c') match {
      case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC), 0) // c-p-r
      case 1 => intcode.memory(intcode.pointer + offsetC) // c-i-r
    }
  }

  def actionAdd(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
      relativeBase = intCode.relativeBase,
      memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) + bParam(instruction, intCode)),
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionMultiply(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
      relativeBase = intCode.relativeBase,
      memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) * bParam(instruction, intCode)),
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionInput(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 2,
      relativeBase = intCode.relativeBase,
      memory = intCode.memory.updated(cParam(instruction, intCode), intCode.input),
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionOutput(instruction: Instruction, intCode: IntCode): IntCode = {
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

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def loop(intCode: IntCode): IntCode = {
      val instruction = pad5(intCode.memory(intCode.pointer))
      instruction('e') match {
        case 1 =>
          loop(actionAdd(instruction, intCode))
        case 2 =>
          loop(actionMultiply(instruction, intCode))
        case 3 =>
          loop(actionInput(instruction, intCode))
        case 4 =>
          loop(actionOutput(instruction, intCode))
        case 9 =>
          intCode
        case _ =>
          throw new Exception("Unknown opCode")
      }
    }

    loop(intCode)
  }
}

//  def opCode(intCode: IntCode): IntCode = {
//    @tailrec
//    def recur(intCode: IntCode): IntCode = {
//      if (intCode.isStopped) {
//        intCode
//      } else {
//        val instruction: Instruction = pad5(intCode.memory(intCode.pointer))
//        instruction('e') match {
//          case 9 =>
//            if (instruction('d') == 9)
//              intCode.copy(isStopped = true) else {
//              recur(IntCode(
//                input = intCode.input,
//                output = intCode.output,
//                phase = intCode.phase,
//                pointer = intCode.pointer + 2,
//                relativeBase = cParam(instruction, intCode) + intCode.relativeBase,
//                memory = intCode.memory,
//                isStopped = intCode.isStopped,
//                doesRecur = intCode.doesRecur))
//            }
//          case 1 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = intCode.pointer + 4,
//              relativeBase = intCode.relativeBase,
//              memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) + bParam(instruction, intCode)),
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//          case 2 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = intCode.pointer + 4,
//              relativeBase = intCode.relativeBase,
//              memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) * bParam(instruction, intCode)),
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//          case 3 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = intCode.pointer + 2,
//              relativeBase = intCode.relativeBase,
//              memory =
//                if (intCode.phase >= 0 && intCode.phase <= 9) {
//                  if (intCode.pointer == 0) {
//                    intCode.memory.updated(cParam(instruction, intCode), intCode.phase)
//                  } else {
//                    intCode.memory.updated(cParam(instruction, intCode), intCode.input)
//                  }
//                } else {
//                  intCode.memory.updated(cParam(instruction, intCode), intCode.input)
//                },
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//          case 4 =>
//            if (intCode.doesRecur) {
//              recur(IntCode(
//                input = intCode.input,
//                output = cParam(instruction, intCode),
//                phase = intCode.phase,
//                pointer = intCode.pointer + 2,
//                relativeBase = intCode.relativeBase,
//                memory = intCode.memory,
//                isStopped = intCode.isStopped,
//                doesRecur = intCode.doesRecur))
//            } else {
//              IntCode(
//                input = intCode.input,
//                output = cParam(instruction, intCode),
//                phase = intCode.phase,
//                pointer = intCode.pointer + 2,
//                relativeBase = intCode.relativeBase,
//                memory = intCode.memory,
//                isStopped = intCode.isStopped,
//                doesRecur = intCode.doesRecur)
//            }
//          case 5 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = if (cParam(instruction, intCode) == 0) {
//                intCode.pointer + 3
//              } else {
//                bParam(instruction, intCode)
//              },
//              relativeBase = intCode.relativeBase,
//              memory = intCode.memory,
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//          case 6 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = if (cParam(instruction, intCode) != 0) {
//                intCode.pointer + 3
//              } else {
//                bParam(instruction, intCode)
//              },
//              relativeBase = intCode.relativeBase,
//              memory = intCode.memory,
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//          case 7 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = intCode.pointer + 4,
//              relativeBase = intCode.relativeBase,
//              memory = if (cParam(instruction, intCode) < bParam(instruction, intCode)) {
//                intCode.memory.updated(aParam(instruction, intCode), 1)
//              } else {
//                intCode.memory.updated(aParam(instruction, intCode), 0)
//              },
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//          case 8 =>
//            recur(IntCode(
//              input = intCode.input,
//              output = intCode.output,
//              phase = intCode.phase,
//              pointer = intCode.pointer + 4,
//              relativeBase = intCode.relativeBase,
//              memory = if (cParam(instruction, intCode) == bParam(instruction, intCode)) {
//                intCode.memory.updated(aParam(instruction, intCode), 1)
//              } else {
//                intCode.memory.updated(aParam(instruction, intCode), 0)
//              },
//              isStopped = intCode.isStopped,
//              doesRecur = intCode.doesRecur))
//        }
//      }
//    }
//
//    recur(intCode)
//  }
//}
