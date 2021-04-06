//$ amm --predef foo.sc

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}

type FilePath = String
type Pointer = Int
type Memory = TreeMap[Int, Int]
type RelativeBase = Int
type Instruction = Array[Int]
type Address = Int
type Value = Int
type AddressAsValue = Int

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

def pad5(rawInstruction: Int): Instruction = {
  "%05d".format(rawInstruction).getBytes.map(_ - 48)
}

// (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]

// ABCDE
// 01002

// a b or c = left-to-right position after 2 digit opcode
// P I or R = position, immediate or relative mode
// r or w = read or write

case class IntCode(input: Value, output: Value, pointer: Address, memory: Memory)

object IntCode {
  def aPw(pointer: Pointer)(memory: Memory): AddressAsValue = memory(pointer + 3)

  def bPrbRr(pointer: Pointer)(memory: Memory)(relativeBase: RelativeBase): Value =
    memory.getOrElse((pointer + 2) + relativeBase, 0)

  def cPrcRr(pointer: Pointer)(memory: Memory)(relativeBase: RelativeBase): Value =
    memory.getOrElse((pointer + 1) + relativeBase, 0)

  def cPwcIr(pointer: Pointer)(memory: Memory): AddressAsValue = memory(pointer + 1)

  def bIr(pointer: Pointer)(memory: Memory): AddressAsValue = memory(pointer + 2)

  def aRw(pointer: Pointer)(memory: Memory)(relativeBase: RelativeBase): AddressAsValue =
    memory((pointer + 3) + relativeBase)

  def cRw(pointer: Pointer)(memory: Memory)(relativeBase: RelativeBase): AddressAsValue =
    memory((pointer + 1) + relativeBase)

  //
  //  def bIr(intCode: IntCode): Int = intCode.pointer + 2
  //
  //  def cPr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 1)
  //
  //  def cPwcIr(intCode: IntCode): Int = intCode.pointer + 1
  //
  //  def paramMakerA(intCode: IntCode): Int = {
  //    pad5(intCode.memory(intCode.pointer)) match {
  //      case Array(0, _, _, _, _) => aPw(intCode)
  //    }
  //  }
  //
  //  def paramMakerB(intCode: IntCode): Int = {
  //    pad5(intCode.memory(intCode.pointer)) match {
  //      case Array(_, 0, _, _, _) => bPr(intCode)
  //      case Array(_, 1, _, _, _) => bIr(intCode)
  //    }
  //  }
  //
  //  def paramMakerC(intCode: IntCode): Int = {
  //    pad5(intCode.memory(intCode.pointer)) match {
  //      case Array(_, _, _, _, 3) => cPwcIr(intCode)
  //      case _ => pad5(intCode.memory(intCode.pointer)) match {
  //        case Array(_, _, 0, _, _) => cPr(intCode)
  //        case Array(_, _, 1, _, _) => cPwcIr(intCode)
  //      }
  //    }
  //  }
  //
  //  def opCode(intCode: IntCode): IntCode = {
  //    @tailrec
  //    def recur(intCode: IntCode): IntCode = {
  //      pad5(intCode.memory(intCode.pointer)) match {
  //        case Array(_, _, _, _, 1) =>
  //          recur(IntCode(
  //            input = intCode.input,
  //            output = intCode.output,
  //            pointer = intCode.pointer + 4,
  //            memory = intCode.memory.updated(intCode.memory(paramMakerA(intCode)), intCode.memory(paramMakerC(intCode)) + intCode.memory(paramMakerB(intCode)))))
  //        case Array(_, _, _, _, 2) =>
  //          recur(IntCode(
  //            input = intCode.input,
  //            output = intCode.output,
  //            pointer = intCode.pointer + 4,
  //            memory = intCode.memory.updated(intCode.memory(paramMakerA(intCode)), intCode.memory(paramMakerC(intCode)) * intCode.memory(paramMakerB(intCode)))))
  //        case Array(_, _, _, _, 3) =>
  //          recur(IntCode(
  //            input = intCode.input,
  //            output = intCode.output,
  //            pointer = intCode.pointer + 2,
  //            memory = intCode.memory.updated(paramMakerC(intCode), intCode.input)))
  //        case Array(_, _, _, _, 4) =>
  //          recur(IntCode(
  //            input = intCode.input,
  //            output = intCode.memory(paramMakerC(intCode)),
  //            pointer = intCode.pointer + 2,
  //            memory = intCode.memory))
  //        case Array(_, _, _, _, 9) => intCode
  //        case _ => IntCode(0, 0, 0, Vector(0))
  //      }
  //    }
  //
  //    recur(intCode)
  //  }
}
