//$ amm --predef src/main/scala/day05.sc

import $file.intcode

// part A
val memory = intcode.makeMemory("/home/eric/scala-projects/advent-19-scala/src/main/resources/day05.csv")

val ic: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 1, output = 0, phase = 999, pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = true))
val answer: Int = ic.output
println(s"Answer Part A: $answer")

// Answer Part A: 9025675

// part B
//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 999, pointer = 0, relativeBase = 0, memory = memory, isStopped = false, doesRecur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754
