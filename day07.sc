import $file.intcode

import scala.collection.immutable.TreeMap

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day07.csv")

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
//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754
