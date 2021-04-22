import $file.intcode

import scala.collection.immutable.TreeMap

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day07.csv")

val possibilities: IndexedSeq[List[Int]] = for (a <- 0 to 4;
                                                b <- 0 to 4;
                                                c <- 0 to 4;
                                                d <- 0 to 4;
                                                e <- 0 to 4
                                                if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
yield List(a, b, c, d, e)

//val ic: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 1, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer: Int = ic.output


//println(s"Answer Part A: $answer")

// Answer Part A: 9025675

// part B
//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754
