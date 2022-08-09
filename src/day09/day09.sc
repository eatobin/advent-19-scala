//$ amm --predef day09.sc

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

object IntCode {
  private val offsetC: Int = 1
  private val offsetB: Int = 2
  private val offsetA: Int = 3

  def aParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('a') match {
      case 0 => intcode.memory(intcode.pointer + offsetA) // a-p-w
      case 2 => intcode.memory(intcode.pointer + offsetA) + intcode.relativeBase // a-r-w
    }
  }

  def bParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('b') match {
      case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetB), 0) // b-p-r
      case 1 => intcode.memory(intcode.pointer + offsetB) // b-i-r
      case 2 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetB) + intcode.relativeBase, 0) // b-r-r
    }
  }

  def cParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('e') match {
      case 3 =>
        instruction('c') match {
          case 0 => intcode.memory(intcode.pointer + offsetC) // c-p-w
          case 2 => intcode.memory(intcode.pointer + offsetC) + intcode.relativeBase // c-r-w
        }
      case _ =>
        instruction('c') match {
          case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC), 0) // c-p-r
          case 1 => intcode.memory(intcode.pointer + offsetC) // c-i-r
          case 2 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC) + intcode.relativeBase, 0) // c-r-r
        }
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

  def actionJumpIfTrue(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = cParam(instruction, intCode),
      phase = intCode.phase,
      pointer = if (cParam(instruction, intCode) != 0) {
        bParam(instruction, intCode)
      } else {
        intCode.pointer + 3
      },
      relativeBase = intCode.relativeBase,
      memory = intCode.memory,
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionJumpIfFalse(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = cParam(instruction, intCode),
      phase = intCode.phase,
      pointer = if (cParam(instruction, intCode) == 0) {
        bParam(instruction, intCode)
      } else {
        intCode.pointer + 3
      },
      relativeBase = intCode.relativeBase,
      memory = intCode.memory,
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionLessThan(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = cParam(instruction, intCode),
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
      relativeBase = intCode.relativeBase,
      memory = if (cParam(instruction, intCode) < bParam(instruction, intCode)) {
        intCode.memory.updated(aParam(instruction, intCode), 1)
      } else {
        intCode.memory.updated(aParam(instruction, intCode), 0)
      },
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionEquals(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = cParam(instruction, intCode),
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
      relativeBase = intCode.relativeBase,
      memory = if (cParam(instruction, intCode) == bParam(instruction, intCode)) {
        intCode.memory.updated(aParam(instruction, intCode), 1)
      } else {
        intCode.memory.updated(aParam(instruction, intCode), 0)
      },
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionHalt(intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer,
      relativeBase = intCode.relativeBase,
      memory = intCode.memory,
      isStopped = true,
      doesRecur = intCode.doesRecur)
  }

  def actionRelativeBase(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 2,
      relativeBase = cParam(instruction, intCode) + intCode.relativeBase,
      memory = intCode.memory,
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def loop(intCode: IntCode): IntCode = {
      if (intCode.isStopped) {
        intCode
      } else {
        val instruction = pad5(intCode.memory(intCode.pointer))
        instruction('e') match {
          case 9 =>
            if (instruction('d') == 9) {
              actionHalt(intCode)
            } else {
              loop(actionRelativeBase(instruction, intCode))
            }
          case 1 =>
            loop(actionAdd(instruction, intCode))
          case 2 =>
            loop(actionMultiply(instruction, intCode))
          case 3 =>
            loop(actionInput(instruction, intCode))
          case 4 =>
            if (intCode.doesRecur) {
              loop(actionOutput(instruction, intCode))
            } else {
              actionOutput(instruction, intCode)
            }
          case 5 =>
            loop(actionJumpIfTrue(instruction, intCode))
          case 6 =>
            loop(actionJumpIfFalse(instruction, intCode))
          case 7 =>
            loop(actionLessThan(instruction, intCode))
          case 8 =>
            loop(actionEquals(instruction, intCode))
          case _ =>
            throw new Exception("Unknown opCode")
        }
      }
    }

    loop(intCode)
  }
}

// part A
val memory = makeMemory("day09.csv")

val ic = IntCode.opCode(IntCode(input = 1, output = 0, phase = 999, pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = true))
val answer = ic
println(s"Answer Part A: $answer")

// Answer Part A: 3780860499

// part B
