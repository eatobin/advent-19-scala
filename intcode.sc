//$ amm --predef intcode.sc

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, TreeMap}
import scala.io.{BufferedSource, Source}

type FilePath = String
type Memory = TreeMap[Int, Int]
type Instruction = ListMap[Char, Int]
type Address = Int
type Value = Int
type RelativeBase = Int

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

def pad5(op: Int): Instruction = {
  val inBytes = "%05d".format(op).getBytes.map(_ - 48)
  val inMap = Array('a', 'b', 'c', 'd', 'e').zip(inBytes).toMap
  ListMap(inMap.toSeq.sortBy(_._1): _*)
}

// (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]

// ABCDE
// 01002

// a b or c = left-to-right position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

case class IntCode(input: Value, output: Value, pointer: Address, relativeBase: RelativeBase, memory: Memory)

object IntCode {
  def aPw(intCode: IntCode): Address = intCode.memory(intCode.pointer + 3)

  def bPrbRr(intCode: IntCode): Value =
    intCode.memory.getOrElse(intCode.memory(intCode.pointer + 2) + intCode.relativeBase, 0)

  def cPrcRr(intCode: IntCode): Value =
    intCode.memory.getOrElse(intCode.memory(intCode.pointer + 1) + intCode.relativeBase, 0)

  def cPwcIr(intCode: IntCode): Address = intCode.memory(intCode.pointer + 1)

  def bIr(intCode: IntCode): Address = intCode.memory(intCode.pointer + 2)

  def aRw(intCode: IntCode): Address = intCode.memory(intCode.pointer + 3) + intCode.relativeBase

  def cRw(intCode: IntCode): Address = intCode.memory(intCode.pointer + 1) + intCode.relativeBase

  def addressMakerC(intCode: IntCode): Address = {
    pad5(intCode.memory(intCode.pointer))('e') match {
      case 1 | 2 | 4 | 5 | 6 | 7 | 8 | 9 =>
        pad5(intCode.memory(intCode.pointer))('c') match {
          case 1 => cPwcIr(intCode)
          case 0 | 2 => cPrcRr(intCode)
        }
      case 3 =>
        pad5(intCode.memory(intCode.pointer))('c') match {
          case 0 => cPwcIr(intCode)
          case 2 => cRw(intCode)
        }
    }
  }

  def addressMakerB(intCode: IntCode): Address = {
    pad5(intCode.memory(intCode.pointer))('b') match {
      case 0 | 2 => bPrbRr(intCode)
      case 1 => bIr(intCode)
    }
  }

  def addressMakerA(intCode: IntCode): Address = {
    pad5(intCode.memory(intCode.pointer))('a') match {
      case 0 => aPw(intCode)
      case 2 => aRw(intCode)
    }
  }

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      pad5(intCode.memory(intCode.pointer))('e') match {
        case 1 =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 4,
            relativeBase = intCode.relativeBase,
            memory = intCode.memory.updated(addressMakerA(intCode), addressMakerC(intCode) + addressMakerB(intCode))))
        case 2 =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 4,
            relativeBase = intCode.relativeBase,
            memory = intCode.memory.updated(addressMakerA(intCode), addressMakerC(intCode) * addressMakerB(intCode))))
        case 3 =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 2,
            relativeBase = intCode.relativeBase,
            memory = intCode.memory.updated(addressMakerC(intCode), intCode.input)))
        case 4 =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.memory(addressMakerC(intCode)),
            pointer = intCode.pointer + 2,
            relativeBase = intCode.relativeBase,
            memory = intCode.memory))
        case 9 => intCode
      }
    }

    recur(intCode)
  }
}
