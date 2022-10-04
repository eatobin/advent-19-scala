//$ amm --predef day07.sc

//> using scala "2.13.9"

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable
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

final case class IntCode(input: Int, output: Int, phase: Int, pointer: Int, memory: Memory, isStopped: Boolean, doesRecur: Boolean)

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
    instruction('e') match {
      case 3 =>
        instruction('c') match {
          case 0 => intcode.memory(intcode.pointer + offsetC) // c-p-w
        }
      case _ =>
        instruction('c') match {
          case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC), 0) // c-p-r
          case 1 => intcode.memory(intcode.pointer + offsetC) // c-i-r
        }
    }
  }


  def actionAdd(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
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
      memory = intCode.memory,
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionJumpIfTrue(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = if (cParam(instruction, intCode) != 0) {
        bParam(instruction, intCode)
      } else {
        intCode.pointer + 3
      },
      memory = intCode.memory,
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionJumpIfFalse(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = if (cParam(instruction, intCode) == 0) {
        bParam(instruction, intCode)
      } else {
        intCode.pointer + 3
      },
      memory = intCode.memory,
      isStopped = intCode.isStopped,
      doesRecur = intCode.doesRecur)
  }

  def actionLessThan(instruction: Instruction, intCode: IntCode): IntCode = {
    IntCode(
      input = intCode.input,
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
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
      output = intCode.output,
      phase = intCode.phase,
      pointer = intCode.pointer + 4,
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
      memory = intCode.memory,
      isStopped = true,
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
              actionHalt(intCode)
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
val memory = makeMemory("day07.csv")

val possibilities: Seq[TreeMap[Char, Int]] =
  for (a <- 0 to 4;
       b <- 0 to 4;
       c <- 0 to 4;
       d <- 0 to 4;
       e <- 0 to 4
       if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap

def pass(possible: TreeMap[Char, Int])(memory: Memory): Int = {
  IntCode.opCode(IntCode(
    input = IntCode.opCode(IntCode(
      input = IntCode.opCode(IntCode(
        input = IntCode.opCode(IntCode(
          input = IntCode.opCode(IntCode(
            input = 0,
            output = 0,
            phase = possible('a'),
            pointer = 0,
            memory = memory,
            isStopped = false,
            doesRecur = true)).output,
          output = 0,
          phase = possible('b'),
          pointer = 0,
          memory = memory,
          isStopped = false,
          doesRecur = true)).output,
        output = 0,
        phase = possible('c'),
        pointer = 0,
        memory = memory,
        isStopped = false,
        doesRecur = true)).output,
      output = 0,
      phase = possible('d'),
      pointer = 0,
      memory = memory,
      isStopped = false,
      doesRecur = true)).output,
    output = 0,
    phase = possible('e'),
    pointer = 0,
    memory = memory,
    isStopped = false,
    doesRecur = true)).output
}

def passes(memory: Memory): Seq[Int] = possibilities.map(pass(_)(memory))

val answer: Int = passes(memory).max

println(s"Answer Part A: $answer")

// Answer Part A: 368584

// part B
val possibilities2: Seq[TreeMap[Char, Int]] =
  for (a <- 5 to 9;
       b <- 5 to 9;
       c <- 5 to 9;
       d <- 5 to 9;
       e <- 5 to 9
       if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap

def makeAnAmpPass(possibility: TreeMap[Char, Int])(memory: Memory): mutable.Map[Int, IntCode] = {
  val fiveAmps: mutable.Map[Int, IntCode] = mutable.Map(
    1 -> IntCode(input = 0, output = 0, phase = possibility('a'), pointer = 0, memory = memory, isStopped = false, doesRecur = false),
    2 -> IntCode(input = 0, output = 0, phase = possibility('b'), pointer = 0, memory = memory, isStopped = false, doesRecur = false),
    3 -> IntCode(input = 0, output = 0, phase = possibility('c'), pointer = 0, memory = memory, isStopped = false, doesRecur = false),
    4 -> IntCode(input = 0, output = 0, phase = possibility('d'), pointer = 0, memory = memory, isStopped = false, doesRecur = false),
    5 -> IntCode(input = 0, output = 0, phase = possibility('e'), pointer = 0, memory = memory, isStopped = false, doesRecur = false)
  )
  fiveAmps
}

def toAmpsList(possibilitiesList: Seq[TreeMap[Char, Int]])(memory: Memory): Seq[mutable.Map[Int, IntCode]] = {
  possibilitiesList.map(makeAnAmpPass(_)(memory))
}

def runner(fiveAmps: mutable.Map[Int, IntCode]): Int = {
  @tailrec
  def loop(amps: mutable.Map[Int, IntCode], currentAmpNo: Int, nextAmpNo: Int): Int = {
    if (currentAmpNo == 5 && amps(currentAmpNo).isStopped) {
      amps(currentAmpNo).output
    } else {
      amps(currentAmpNo) = IntCode.opCode(amps(currentAmpNo))
      amps(nextAmpNo) = amps(nextAmpNo).copy(input = amps(currentAmpNo).output)
      loop(amps = amps, currentAmpNo = nextAmpNo, nextAmpNo = (nextAmpNo % 5) + 1)
    }
  }

  loop(amps = fiveAmps, currentAmpNo = 1, nextAmpNo = 2)
}

val answer2: Int = toAmpsList(possibilities2)(memory).map(runner).max
println(s"Answer Part B: $answer2")

// Answer Part B: 35993240
