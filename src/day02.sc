import $file.intcode

import scala.collection.immutable.TreeMap

val tv: TreeMap[Int, Int] = intcode.makeTV("resources/day02.csv")
println(tv)
