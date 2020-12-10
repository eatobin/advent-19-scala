import $file.intcode

import scala.collection.immutable.TreeMap

val memory: TreeMap[Int, Int] = intcode.makeMemory("resources/day02.csv")
val updatedMemory: TreeMap[Int, Int] = memory ++ List(1 -> 12, 2 -> 2)
val ic: intcode.IntCode = intcode.IntCode.ic(intcode.IntCode(0, updatedMemory))
val answer: Int = ic.memory(0)
println(s"Answer Part A: $answer")
