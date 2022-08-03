//$ amm --predef src/main/scala/day02.sc

import $file.intcode

// part A
val memory = intcode.makeMemory("/home/eric/scala-projects/advent-19-scala/src/main/resources/day02.csv")

def updatedMemory(noun: Int, verb: Int): intcode.Memory = {
  val newNoun = memory.updated(1, noun)
  newNoun.updated(2, verb)
}

val ic = intcode.IntCode.opCode(intcode.IntCode(pointer = 0, memory = updatedMemory(noun = 12, verb = 2)))

val answer = ic.memory(0)

println(s"Answer Part A: $answer")

// Answer Part A: 2890696

// part B
val answer2 = (for {noun <- Range.inclusive(0, 99)
                    verb <- Range.inclusive(0, 99)
                    candidate: Int = intcode.IntCode.opCode(intcode.IntCode(pointer = 0, memory = updatedMemory(noun = noun, verb = verb))).memory(0)
                    if candidate == 19690720
                    } yield (100 * noun) + verb).head

println(s"Answer Part B: $answer2")

// Answer Part B: 8226
