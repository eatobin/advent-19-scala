//$ amm --predef day05.sc

import scala.annotation.tailrec
import scala.io.Source

type FilePath = String
type Memory = Map[Int, Int]
type Instruction = Map[Char, Int]

def makeMemory(file: FilePath): Memory = {
  val bufferedSource = Source.fromFile(file)
  val intArray =
    bufferedSource.mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  bufferedSource.close
  Iterator.from(0).zip(intArray).toMap
}

def charToInt(aChar: Byte): Int = {
  if aChar < 48 || aChar > 57 then throw new Exception("Char is not an integer")
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

final case class IntCode(input: Int, output: Int, pointer: Int, memory: Memory)

object IntCode {
  private val offsetC: Int = 1
  private val offsetB: Int = 2
  private val offsetA: Int = 3

  def aParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('a') match {
      case 0 => { intcode.memory(intcode.pointer + offsetA) } // a-p-w
    }
  }

  def bParam(instruction: Instruction, intcode: IntCode): Int =
    instruction('b') match {
      case 0 => {
        intcode.memory.getOrElse(
          intcode.memory(intcode.pointer + offsetB),
          0
        )
      } // b-p-r
      case 1 => { intcode.memory(intcode.pointer + offsetB) } // b-i-r
    }

  def cParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('e') match {
      case 3 => {
        instruction('c') match {
          case 0 => { intcode.memory(intcode.pointer + offsetC) } // c-p-w
        }
      }
      case _ => {
        instruction('c') match {
          case 0 => {
            intcode.memory.getOrElse(
              intcode.memory(intcode.pointer + offsetC),
              0
            )
          } // c-p-r
          case 1 => { intcode.memory(intcode.pointer + offsetC) } // c-i-r
        }
      }
    }
  }

  def actionAdd(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer = intCode.pointer + 4,
      memory = intCode.memory.updated(
        aParam(instruction, intCode),
        cParam(instruction, intCode) + bParam(instruction, intCode)
      )
    )

  def actionMultiply(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer = intCode.pointer + 4,
      memory = intCode.memory.updated(
        aParam(instruction, intCode),
        cParam(instruction, intCode) * bParam(instruction, intCode)
      )
    )

  def actionInput(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer = intCode.pointer + 2,
      memory =
        intCode.memory.updated(cParam(instruction, intCode), intCode.input)
    )

  def actionOutput(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = cParam(instruction, intCode),
      pointer = intCode.pointer + 2,
      memory = intCode.memory
    )

  def actionJumpIfTrue(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer =
        if cParam(instruction, intCode) != 0 then bParam(instruction, intCode)
        else intCode.pointer + 3,
      memory = intCode.memory
    )

  def actionJumpIfFalse(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer =
        if cParam(instruction, intCode) == 0 then bParam(instruction, intCode)
        else intCode.pointer + 3,
      memory = intCode.memory
    )

  def actionLessThan(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer = intCode.pointer + 4,
      memory =
        if cParam(instruction, intCode) < bParam(instruction, intCode) then
          intCode.memory.updated(aParam(instruction, intCode), 1)
        else intCode.memory.updated(aParam(instruction, intCode), 0)
    )

  def actionEquals(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      input = intCode.input,
      output = intCode.output,
      pointer = intCode.pointer + 4,
      memory =
        if cParam(instruction, intCode) == bParam(instruction, intCode) then
          intCode.memory.updated(aParam(instruction, intCode), 1)
        else intCode.memory.updated(aParam(instruction, intCode), 0)
    )

  def opCode(intCode: IntCode): IntCode =
    @tailrec
    def loop(intCode: IntCode): IntCode =
      val instruction = pad5(intCode.memory(intCode.pointer))
      instruction('e') match
        case 1 =>
          loop(actionAdd(instruction, intCode))
        case 2 =>
          loop(actionMultiply(instruction, intCode))
        case 3 =>
          loop(actionInput(instruction, intCode))
        case 4 =>
          loop(actionOutput(instruction, intCode))
        case 5 =>
          loop(actionJumpIfTrue(instruction, intCode))
        case 6 =>
          loop(actionJumpIfFalse(instruction, intCode))
        case 7 =>
          loop(actionLessThan(instruction, intCode))
        case 8 =>
          loop(actionEquals(instruction, intCode))
        case 9 =>
          intCode
        case _ =>
          throw new Exception("Unknown opCode")
      end match
    end loop

    loop(intCode)
  end opCode
}

// part A
val memory = makeMemory("day05.csv")

val ic: IntCode =
  IntCode.opCode(IntCode(input = 1, output = 0, pointer = 0, memory = memory))
val answer: Int = ic.output
println(s"Answer Part A: $answer")

// Answer Part A: 9025675

// part B
val ic2: IntCode =
  IntCode.opCode(IntCode(input = 5, output = 0, pointer = 0, memory = memory))
val answer2: Int = ic2.output
println(s"Answer Part B: $answer2")

// Answer Part B: 11981754
