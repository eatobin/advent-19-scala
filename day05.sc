import $file.intcode5

// part A
val memory: Vector[Int] = intcode5.makeMemory("resources/day05.csv")
val ic: intcode5.IntCode = intcode5.IntCode.opCode(intcode5.IntCode(input = 1, output = 0, pointer = 0, memory = memory))
val answer: Int = ic.output
println(s"Answer Part A: $answer")

// Answer Part A: 9025675

// part B
//val answer2: Int = (for {noun <- Range.inclusive(0, 99)
//                         verb <- Range.inclusive(0, 99)
//                         candidate: Int = intcode5.IntCode.opCode(intcode5.IntCode(input = 0, output = 0, pointer = 0, memory = updatedMemory(noun = noun)(verb = verb))).memory(0)
//                         if candidate == 19690720
//                         } yield (100 * noun) + verb).head
//
//println(s"Answer Part B: $answer2")

// Answer Part B: 8226
