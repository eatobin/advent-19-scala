import $file.intcode

import scala.collection.immutable.TreeMap

// part A
type Memory = TreeMap[Int, Int]

val memory: Memory = intcode.makeMemory("resources/day02.csv")
def updatedMemory(noun: Int)(verb: Int): Memory = {
  val newNoun = memory.updated(1, noun)
  newNoun.updated(2, verb)
}
val ic: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 0, output = 0, phase = 999, pointer = 0, relativeBase = 0, memory = updatedMemory(noun = 12)(verb = 2), isStopped = false, doesRecur = true))
val answer: Int = ic.memory(0)
println(s"Answer Part A: $answer")

// Answer Part A: 2890696

// part B
val answer2: Int = (for {noun <- Range.inclusive(0, 99)
                         verb <- Range.inclusive(0, 99)
                         candidate: Int = intcode.IntCode.opCode(intcode.IntCode(input = 0, output = 0, phase = 999, pointer = 0, relativeBase = 0, memory = updatedMemory(noun = noun)(verb = verb), isStopped = false, doesRecur = true)).memory(0)
                         if candidate == 19690720
                         } yield (100 * noun) + verb).head

println(s"Answer Part B: $answer2")

// Answer Part B: 8226
