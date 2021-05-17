//$ amm --predef day09.sc

import $file.intcode

import scala.collection.immutable.TreeMap

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day09.csv")



// Answer Part A: 368584

// part B


// Answer Part B: 35993240
