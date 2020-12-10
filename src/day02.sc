import $file.intcode

import scala.collection.immutable.TreeMap

val memory: TreeMap[Int, Int] = intcode.makeMemory("resources/t1.csv")
//println(memory)
println(intcode.IntCode.runCode(intcode.IntCode(0, memory)))

//def tester(x: Int, s: String): String = s"$s $x dogs?"
//println(tester(s = "do you have", x = 8))

//def updatedMemory(noun: Int, verb: Int, memory: TreeMap[Int, Int]): TreeMap[Int, Int] = {
//  memory ++ List(1 -> noun, 2 -> verb)
//}
//
//println(updatedMemory(111, 222, tv))
