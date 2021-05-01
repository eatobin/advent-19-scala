import $file.intcode

import scala.collection.immutable.TreeMap
import scala.collection.mutable

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day07.csv")

val shortMem = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0))
val myMem = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 4))

val possibilities: List[TreeMap[Char, Int]] =
  (for (a <- 0 to 4;
        b <- 0 to 4;
        c <- 0 to 4;
        d <- 0 to 4;
        e <- 0 to 4
        if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap).toList

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
            stopped = false,
            recur = true)).output,
          output = 0,
          phase = possible('b'),
          pointer = 0,
          relativeBase = 0,
          memory = memory,
          stopped = false,
          recur = true)).output,
        output = 0,
        phase = possible('c'),
        pointer = 0,
        relativeBase = 0,
        memory = memory,
        stopped = false,
        recur = true)).output,
      output = 0,
      phase = possible('d'),
      pointer = 0,
      relativeBase = 0,
      memory = memory,
      stopped = false,
      recur = true)).output,
    output = 0,
    phase = possible('e'),
    pointer = 0,
    relativeBase = 0,
    memory = memory,
    stopped = false,
    recur = true)).output
}

def passes(memory: Memory): List[Int] = possibilities.map(pass(_)(memory))

val answer: Int = passes(memory).max

println(s"Answer Part A: $answer")

// Answer Part A: 368584

// part B
val possibilities2: List[TreeMap[Char, Int]] =
  (for (a <- 5 to 9;
        b <- 5 to 9;
        c <- 5 to 9;
        d <- 5 to 9;
        e <- 5 to 9
        if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap).toList

var fiveAmps: mutable.Map[Int, intcode.IntCode] =
  mutable.Map(1 -> intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false))



def makeAnAmpPass(possibility: TreeMap[Char, Int])(memory: Memory): mutable.Map[Int, intcode.IntCode] = {
  val fiveAmps: mutable.Map[Int, intcode.IntCode] = mutable.Map(
    1 -> intcode.IntCode(input = 0, output = 0, phase = possibility('a'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    2 -> intcode.IntCode(input = 0, output = 0, phase = possibility('b'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    3 -> intcode.IntCode(input = 0, output = 0, phase = possibility('c'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    4 -> intcode.IntCode(input = 0, output = 0, phase = possibility('d'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    5 -> intcode.IntCode(input = 0, output = 0, phase = possibility('e'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false)
  )
  fiveAmps
}


//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754

//  amp1 = amp1.copy(phase = possibility('a'), memory = memory)
//  amp2 = amp2.copy(phase = possibility('b'), memory = memory)
//  amp3 = amp3.copy(phase = possibility('c'), memory = memory)
//  amp4 = amp4.copy(phase = possibility('d'), memory = memory)
//  amp5 = amp5.copy(phase = possibility('e'), memory = memory)
