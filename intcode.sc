//$ amm --predef foo.sc

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}

type FilePath = String
type Pointer = Int
type Memory = TreeMap[Int, Int]
type RelativeBase = Int
type Instruction = Array[Int]
type Value = Int
type Address = Int

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
  def opCode(intCode: IntCode): IntCode = {
    @tailrec
    def recur(intCode: IntCode): IntCode = {
      pad5(intCode.memory(intCode.pointer)) match {
        case Array(0, 0, 0, 0, 1) =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 4,
            memory = intCode.memory.updated(intCode.memory(intCode.pointer + 3),
              intCode.memory(intCode.memory(intCode.pointer + 1)) + intCode.memory(intCode.memory(intCode.pointer + 2)))))
        case Array(0, 0, 0, 0, 2) =>
          recur(IntCode(
            input = intCode.input,
            output = intCode.output,
            pointer = intCode.pointer + 4,
            memory = intCode.memory.updated(intCode.memory(intCode.pointer + 3),
              intCode.memory(intCode.memory(intCode.pointer + 1)) * intCode.memory(intCode.memory(intCode.pointer + 2)))))
        case Array(0, 0, 0, 9, 9) => intCode
      }
    }

    recur(intCode)
  }
}


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
//      }

//
//    }
//  }
//}
