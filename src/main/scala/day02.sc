//$ amm --predef day02.sc

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

//type FilePath = String
type Memory = Map[Int, Int]
type Instruction = Map[Char, Int]

val fp: String = "../resources/day02.csv"

def makeMemory(file: String): Memory = {
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

def charToInt(aChar: Byte): Byte = {
  if (aChar < 48 || aChar > 57)
    throw new Exception("Char is not an integer")
  else (aChar - 48).toByte
}

def pad5(op: Int): Instruction = {
  val inBytes: Array[Int] = "%05d".format(op).getBytes.map(charToInt)
  Array('a', 'b', 'c', 'd', 'e').zip(inBytes).toMap
}

case class IntCode(pointer: Int, memory: Memory)

object IntCode {
  private val offsetC: Int = 1
  private val offsetB: Int = 2
  private val offsetA: Int = 3

  def aParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('a') match {
      // a-p-w
      case 0 => intcode.memory(intcode.pointer + offsetA)
    }
  }

  def bParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('b') match {
      // b-p-r
      case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetB), 0)
    }
  }

  def cParam(instruction: Instruction, intcode: IntCode): Int = {
    instruction('c') match {
      // c-p-r
      case 0 => intcode.memory.getOrElse(intcode.memory(intcode.pointer + offsetC), 0)
    }
  }

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      val instruction: Instruction = pad5(intCode.memory(intCode.pointer))
      instruction('e') match {
        case 1 =>
          recur(IntCode(
            pointer = intCode.pointer + 4,
            memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) + bParam(instruction, intCode))))
        case 2 =>
          recur(IntCode(
            pointer = intCode.pointer + 4,
            memory = intCode.memory.updated(aParam(instruction, intCode), cParam(instruction, intCode) * bParam(instruction, intCode))))
        case _ =>
          intCode
      }
    }

    recur(intCode)
  }
}

// part A
val memory: Memory = makeMemory(fp)
def updatedMemory(noun: Int)(verb: Int): Memory = {
  val newNoun = memory.updated(1, noun)
  newNoun.updated(2, verb)
}
val ic: IntCode = IntCode.opCode(IntCode(pointer = 0, memory = updatedMemory(noun = 12)(verb = 2)))
val answer: Int = ic.memory(0)
println(s"Answer Part A: $answer")

// Answer Part A: 2890696

// part B
val answer2: Int = (for {noun <- Range.inclusive(0, 99)
                         verb <- Range.inclusive(0, 99)
                         candidate: Int = IntCode.opCode(IntCode(pointer = 0, memory = updatedMemory(noun = noun)(verb = verb))).memory(0)
                         if candidate == 19690720
                         } yield (100 * noun) + verb).head

println(s"Answer Part B: $answer2")

// Answer Part B: 8226
