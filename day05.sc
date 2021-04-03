import $file.intcode5

// part A
val memory: Vector[Int] = intcode5.makeMemory("resources/day02.csv")
def updatedMemory(noun: Int)(verb: Int): Vector[Int] = {
  val newNoun = memory.updated(1, noun)
  newNoun.updated(2, verb)
}
val ic: intcode5.IntCode = intcode5.IntCode.opCode(intcode5.IntCode(input = 0, output = 0, pointer = 0, memory = updatedMemory(noun = 12)(verb = 2)))
val answer: Int = ic.memory(0)
println(s"Answer Part A: $answer")

// Answer Part A: 2890696

// part B
val answer2: Int = (for {noun <- Range.inclusive(0, 99)
                         verb <- Range.inclusive(0, 99)
                         candidate: Int = intcode5.IntCode.opCode(intcode5.IntCode(input = 0, output = 0, pointer = 0, memory = updatedMemory(noun = noun)(verb = verb))).memory(0)
                         if candidate == 19690720
                         } yield (100 * noun) + verb).head

println(s"Answer Part B: $answer2")

// Answer Part B: 8226
