//$ amm --predef foo.sc

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

def makeMemory(file: String): Vector[Int] = {
  val bufferedSource: BufferedSource = Source.fromFile(file)
  bufferedSource
    .mkString
    .split(",")
    .map(_.trim)
    .map(_.toInt)
    .toVector
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

case class IntCode(input: Int = 0, output: Int = 0, pointer: Int = 0, memory: Vector[Int])

object IntCode {
  def aPw(intCode: IntCode): Int = intCode.pointer + 3

  def bPr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 2)

  def cPr(intCode: IntCode): Int = intCode.memory(intCode.pointer + 1)

  def cPw(intCode: IntCode): Int = intCode.pointer + 1

//  def opCode(intCode: IntCode): IntCode = {
//    @tailrec
//    def recur(intCode: IntCode): IntCode = {
//      pad5(intCode.memory(intCode.pointer)) match {
//        case "00001" =>
//          val added: Int = intCode.memory(cPr(intCode)) + intCode.memory(bPr(intCode))
//          val newMemory: Vector[Int] = intCode.memory.updated(intCode.memory(aPw(intCode)), added)
//          recur(IntCode(input = intCode.input, output = intCode.output, pointer = intCode.pointer + 4, memory = newMemory))
//        case "00002" =>
//          val multiplied: Int = intCode.memory(cPr(intCode)) * intCode.memory(bPr(intCode))
//          val newMemory: Vector[Int] = intCode.memory.updated(intCode.memory(aPw(intCode)), multiplied)
//          recur(IntCode(input = intCode.input, output = intCode.output, pointer = intCode.pointer + 4, memory = newMemory))
//        case "00003" =>
//          val newMemory: Vector[Int] = intCode.memory.updated(intCode.memory(cPw(intCode)), intCode.input)
//          recur(IntCode(input = intCode.input, output = intCode.output, pointer = intCode.pointer + 2, memory = newMemory))
//        case "00004" =>
//          recur(IntCode(input = intCode.input, output = intCode.memory(cPr(intCode)), pointer = intCode.pointer + 2, memory = intCode.memory))
//        case "00099" => intCode
//      }
//    }
//
//    recur(intCode)
//  }
}
