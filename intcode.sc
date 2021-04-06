//$ amm --predef foo.sc

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}

def makeMemory(file: String): TreeMap[Int, Int] = {
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

def pad5(instruction: Int): Array[Int] = {
  "%05d".format(instruction).getBytes.map(_ - 48)
}

// (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]

// ABCDE
// 01002

// a b or c = left-to-right position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

case class IntCode(input: Int, output: Int, pointer: Int, memory: Vector[Int])

object IntCode {
  def aPw(intCode: IntCode): Int = intCode.pointer + 3

  def bPr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 2)

  def bIr(intCode: IntCode): Int = intCode.pointer + 2

  def cPr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 1)

  def cPwcIr(intCode: IntCode): Int = intCode.pointer + 1

  def paramMakerA(intCode: IntCode): Int = {
    pad5(intCode.memory(intCode.pointer)) match {
      case Array(0, _, _, _, _) => aPw(intCode)
    }
  }

  def paramMakerB(intCode: IntCode): Int = {
    pad5(intCode.memory(intCode.pointer)) match {
      case Array(_, 0, _, _, _) => bPr(intCode)
      case Array(_, 1, _, _, _) => bIr(intCode)
    }
  }

  def paramMakerC(intCode: IntCode): Int = {
    pad5(intCode.memory(intCode.pointer)) match {
      case Array(_, _, _, _, 3) => cPwcIr(intCode)
      case _ => pad5(intCode.memory(intCode.pointer)) match {
        case Array(_, _, 0, _, _) => cPr(intCode)
        case Array(_, _, 1, _, _) => cPwcIr(intCode)
      }
    }
  }

  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      pad5(intCode.memory(intCode.pointer)) match {
        case Array(_, _, _, _, 1) =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 4,
            memory = intCode.memory.updated(intCode.memory(paramMakerA(intCode)), intCode.memory(paramMakerC(intCode)) + intCode.memory(paramMakerB(intCode)))))
        case Array(_, _, _, _, 2) =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 4,
            memory = intCode.memory.updated(intCode.memory(paramMakerA(intCode)), intCode.memory(paramMakerC(intCode)) * intCode.memory(paramMakerB(intCode)))))
        case Array(_, _, _, _, 3) =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 2,
            memory = intCode.memory.updated(paramMakerC(intCode), intCode.input)))
        case Array(_, _, _, _, 4) =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.memory(paramMakerC(intCode)),
            pointer = intCode.pointer + 2,
            memory = intCode.memory))
        case Array(_, _, _, _, 9) => intCode
        case _ => IntCode(0, 0, 0, Vector(0))
      }
    }

    recur(intCode)
  }
}
