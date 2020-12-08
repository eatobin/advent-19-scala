import $file.intcode

import scala.collection.immutable.TreeMap

val tv: TreeMap[Int, Int] = intcode.makeTV("resources/day02.csv")
println(tv)

//def tester(x: Int, s: String): String = s"$s $x dogs?"
//println(tester(s = "do you have", x = 8))

def updatedMemory(noun: Int, verb: Int, memory: TreeMap[Int, Int]): TreeMap[Int, Int] = {
  memory + (1 -> noun, 2 -> verb)
}

println(updatedMemory(111, 222, tv))
