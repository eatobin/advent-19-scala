import $file.intcode

// part A
val memory: Vector[Int] = intcode.makeMemory("resources/day02.csv")
def updatedMemory(noun: Int)(verb: Int): Vector[Int] = {
  val newNoun = memory.updated(1, noun)
  newNoun.updated(2, verb)
}
val ic: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(memory = updatedMemory(noun = 12)(verb = 2)))
val answer: Int = ic.output
println(s"Answer Part A: $answer")

// Answer Part A: 2890696

// part B
val answer2: Int = (for {noun <- Range.inclusive(0, 99)
                         verb <- Range.inclusive(0, 99)
                         candidate: Int = intcode.IntCode.opCode(intcode.IntCode(memory = updatedMemory(noun = noun)(verb = verb))).output
                         if candidate == 19690720
                         } yield (100 * noun) + verb).head

println(s"Answer Part B: $answer2")

// Answer Part B: 8226
