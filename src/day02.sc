import $file.intcode

import scala.collection.SortedMap

val tv: SortedMap[Int, Int] = intcode.makeTV("resources/day02.csv")
println(tv)
