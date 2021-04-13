import $file.intcode

import scala.collection.immutable.TreeMap

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day05.csv")
val ic: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 1, output = 0, pointer = 0, relativeBase = 0, memory = memory))
val answer: Int = ic.output
println(s"Answer Part A: $answer")

// Answer Part A: 9025675

// part B
val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, pointer = 0, relativeBase = 0, memory = memory))
val answer2: Int = ic2.output
println(s"Answer Part B: $answer2")

// Answer Part B: 11981754
