import $file.intcode

import scala.collection.immutable.TreeMap

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day07.csv")

val shortMem = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0))
val myMem = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 4))

val possibilities: List[Map[Char, Int]] =
  (for (a <- 0 to 4;
        b <- 0 to 4;
        c <- 0 to 4;
        d <- 0 to 4;
        e <- 0 to 4
        if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap).toList

def pass(possible: Map[Char, Int])(memory: Memory): Int = {
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
val possibilities2: List[Map[Char, Int]] =
  (for (a <- 5 to 9;
        b <- 5 to 9;
        c <- 5 to 9;
        d <- 5 to 9;
        e <- 5 to 9
        if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap).toList

var amp1: intcode.IntCode = intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)
var amp2: intcode.IntCode = intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)
var amp3: intcode.IntCode = intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)
var amp4: intcode.IntCode = intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)
var amp5: intcode.IntCode = intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)

val amps: Array[intcode.IntCode] = Array(amp1, amp2, amp3, amp4, amp5)

def makeAnAmpPass(possibility: Map[Char, Int])(memory: Memory): Unit = {
  amp1 = amp1.copy(phase = possibility('a'), memory = memory)
  amp2 = amp2.copy(phase = possibility('b'), memory = memory)
  amp3 = amp3.copy(phase = possibility('c'), memory = memory)
  amp4 = amp4.copy(phase = possibility('d'), memory = memory)
  amp5 = amp5.copy(phase = possibility('e'), memory = memory)
}











//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754


//@ var ampses = scala.collection.mutable.Map[Int, intcode.IntCode]()
//ampses: collection.mutable.Map[Int, intcode.IntCode] = HashMap()
//
//@ ampses(1) = intcode.IntCode(input = 0, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)
//
//@ ampses(2) = intcode.IntCode(input = 2, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = shortMem, stopped = false, recur = false)
//
//
//@ println(ampses)
//HashMap(1 -> IntCode(0,0,0,0,0,TreeMap(0 -> 3, 1 -> 15, 2 -> 3, 3 -> 16, 4 -> 1002, 5 -> 16, 6 -> 10, 7 -> 16, 8 -> 1, 9 -> 16, 10 -> 15, 11 -> 15, 12 -> 4, 13 -> 15, 14 -> 99, 15 -> 0, 16 -> 0),false,false), 2 -> IntCode(2,0,0,0,0,TreeMap(0 -> 3, 1 -> 15, 2 -> 3, 3 -> 16, 4 -> 1002, 5 -> 16, 6 -> 10, 7 -> 16, 8 -> 1, 9 -> 16, 10 -> 15, 11 -> 15, 12 -> 4, 13 -> 15, 14 -> 99, 15 -> 0, 16 -> 0),false,false))
//
//
//@ ampses(1) = ampses(1).copy(input = 9)
//
//
//@ println(ampses)
//HashMap(1 -> IntCode(9,0,0,0,0,TreeMap(0 -> 3, 1 -> 15, 2 -> 3, 3 -> 16, 4 -> 1002, 5 -> 16, 6 -> 10, 7 -> 16, 8 -> 1, 9 -> 16, 10 -> 15, 11 -> 15, 12 -> 4, 13 -> 15, 14 -> 99, 15 -> 0, 16 -> 0),false,false), 2 -> IntCode(2,0,0,0,0,TreeMap(0 -> 3, 1 -> 15, 2 -> 3, 3 -> 16, 4 -> 1002, 5 -> 16, 6 -> 10, 7 -> 16, 8 -> 1, 9 -> 16, 10 -> 15, 11 -> 15, 12 -> 4, 13 -> 15, 14 -> 99, 15 -> 0, 16 -> 0),false,false))
