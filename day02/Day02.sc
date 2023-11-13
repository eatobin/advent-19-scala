// scala-cli Day02.sc

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.io.Source

type FilePath = String
type Memory = Map[Int, Int]
type Instruction = Map[Char, Int]

def makeMemory(file: FilePath): Memory =
  val bufferedSource: BufferedSource = Source.fromFile(file)
  val intArray =
    bufferedSource.mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  bufferedSource.close
  Iterator.from(0).zip(intArray).toMap

def charToInt(aChar: Byte): Int =
  if (aChar < 48 || aChar > 57)
  then throw new Exception("Char is not an integer")
  else aChar - 48

def pad5(op: Int): Instruction =
  val inInts = "%05d".format(op).getBytes.map(charToInt)
  Array('a', 'b', 'c', 'd', 'e').zip(inInts).toMap

// ABCDE
// 01002

// a b or c = left-to-right position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

final case class IntCode(pointer: Int, memory: Memory)

object IntCode:
  val offsetC: Int = 1
  val offsetB: Int = 2
  val offsetA: Int = 3

  def aParam(instruction: Instruction, intcode: IntCode): Int =
    instruction('a') match
      case 0 => intcode.memory(intcode.pointer + offsetA) // a-p-w

  def bParam(instruction: Instruction, intcode: IntCode): Int =
    instruction('b') match
      case 0 =>
        intcode.memory.getOrElse(
          intcode.memory(intcode.pointer + offsetB),
          0
        ) // b-p-r

  def cParam(instruction: Instruction, intcode: IntCode): Int =
    instruction('c') match
      case 0 =>
        intcode.memory.getOrElse(
          intcode.memory(intcode.pointer + offsetC),
          0
        ) // c-p-r

  def actionAdd(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      pointer = intCode.pointer + 4,
      memory = intCode.memory.updated(
        aParam(instruction, intCode),
        cParam(instruction, intCode) + bParam(instruction, intCode)
      )
    )

  def actionMultiply(instruction: Instruction, intCode: IntCode): IntCode =
    IntCode(
      pointer = intCode.pointer + 4,
      memory = intCode.memory.updated(
        aParam(instruction, intCode),
        cParam(instruction, intCode) * bParam(instruction, intCode)
      )
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
        case 9 =>
          intCode
        case _ =>
          throw new Exception("Unknown opCode")

    loop(intCode)

// part A
val memory = makeMemory("Day02.csv")

def updatedMemory(noun: Int, verb: Int): Memory =
  val newNoun = memory.updated(1, noun)
  newNoun.updated(2, verb)

val ic = IntCode.opCode(
  IntCode(pointer = 0, memory = updatedMemory(noun = 12, verb = 2))
)
val answer = ic.memory(0)

println(s"Answer Part A: $answer")

// Answer Part A: 2890696

// part B
val answer2 = (for
  noun <- Range.inclusive(0, 99)
  verb <- Range.inclusive(0, 99)
  candidate: Int = IntCode
    .opCode(
      IntCode(pointer = 0, memory = updatedMemory(noun = noun, verb = verb))
    )
    .memory(0)
  if candidate == 19690720
yield (100 * noun) + verb).head

println(s"Answer Part B: $answer2")

// Answer Part B: 8226
