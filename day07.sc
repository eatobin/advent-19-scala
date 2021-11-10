//$ amm --predef day07.sc

// amm --predef "day07.sc"

import $file.intcode

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day07.csv")

val possibilities: Seq[TreeMap[Char, Int]] =
  for (a <- 0 to 4;
       b <- 0 to 4;
       c <- 0 to 4;
       d <- 0 to 4;
       e <- 0 to 4
       if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap

def pass(possible: TreeMap[Char, Int])(memory: Memory): Int = {
  intcode.IntCode.opCode(intcode.IntCode(
    input = intcode.IntCode.opCode(intcode.IntCode(
      input = intcode.IntCode.opCode(intcode.IntCode(
        input = intcode.IntCode.opCode(intcode.IntCode(
          input = intcode.IntCode.opCode(intcode.IntCode(
            input = 0,
            output = 0,
            phase = possible('a'),
            pointer = 0,
            relativeBase = 0,
            memory = memory,
            isStopped = false,
            doesRecur = true)).output,
          output = 0,
          phase = possible('b'),
          pointer = 0,
          relativeBase = 0,
          memory = memory,
          isStopped = false,
          doesRecur = true)).output,
        output = 0,
        phase = possible('c'),
        pointer = 0,
        relativeBase = 0,
        memory = memory,
        isStopped = false,
        doesRecur = true)).output,
      output = 0,
      phase = possible('d'),
      pointer = 0,
      relativeBase = 0,
      memory = memory,
      isStopped = false,
      doesRecur = true)).output,
    output = 0,
    phase = possible('e'),
    pointer = 0,
    relativeBase = 0,
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

def makeAnAmpPass(possibility: TreeMap[Char, Int])(memory: Memory): mutable.Map[Int, intcode.IntCode] = {
  val fiveAmps: mutable.Map[Int, intcode.IntCode] = mutable.Map(
    1 -> intcode.IntCode(input = 0, output = 0, phase = possibility('a'), pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = false),
    2 -> intcode.IntCode(input = 0, output = 0, phase = possibility('b'), pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = false),
    3 -> intcode.IntCode(input = 0, output = 0, phase = possibility('c'), pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = false),
    4 -> intcode.IntCode(input = 0, output = 0, phase = possibility('d'), pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = false),
    5 -> intcode.IntCode(input = 0, output = 0, phase = possibility('e'), pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = false)
  )
  fiveAmps
}

def toAmpsList(possibilitiesList: Seq[TreeMap[Char, Int]])(memory: Memory): Seq[mutable.Map[Int, intcode.IntCode]] = {
  possibilitiesList.map(makeAnAmpPass(_)(memory))
}

def runner(fiveAmps: mutable.Map[Int, intcode.IntCode]): Int = {
  @tailrec
  def recurXXX(amps: mutable.Map[Int, intcode.IntCode], currentAmpNo: Int, nextAmpNo: Int): Int = {
    if (currentAmpNo == 5 && amps(currentAmpNo).isStopped) {
      amps(currentAmpNo).output
    } else {
      amps(currentAmpNo) = intcode.IntCode.opCode(amps(currentAmpNo))
      amps(nextAmpNo) = amps(nextAmpNo).copy(input = amps(currentAmpNo).output)
      recurXXX(amps = amps, currentAmpNo = nextAmpNo, nextAmpNo = (nextAmpNo % 5) + 1)
    }
  }

  recurXXX(amps = fiveAmps, currentAmpNo = 1, nextAmpNo = 2)
}

val answer2: Int = toAmpsList(possibilities2)(memory).map(runner).max
println(s"Answer Part B: $answer2")

// Answer Part B: 35993240
