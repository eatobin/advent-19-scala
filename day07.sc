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

val short = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0))

def pass(possible: Map[Char, Int])(memory: Memory): Int = {
  intcode.IntCode.opCode(intcode.IntCode(
    input = 0,
    output = 0,
    phase = possible('e'),
    pointer = 0,
    relativeBase = 0,
    memory = memory,
    stopped = false,
    recur = true)).output
}
//  ic.copy(phase = candidate(0))
//  intcode.IntCode.opCode(intcode.IntCode(input = 1, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))

//val ic: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 1, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer: Int = ic.output
//println(possibilities(119)('b'))

//println(s"Answer Part A: $answer")

// Answer Part A: 9025675

// part B
//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754
