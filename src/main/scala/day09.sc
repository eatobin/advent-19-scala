//$ amm --predef day09.sc

import $file.intcode

import scala.collection.immutable.TreeMap

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day09.csv")



// Answer Part A: 368584

// part B


// Answer Part B: 35993240





//(ns advent.day09
//  (:require [advent.intcode :as ic]))
//
//;part a
//(def tv (ic/make-tv "resources/day09.csv"))
//
//
//(def answer ((ic/op-code {:input 1 :output nil :phase nil :pointer 0 :relative-base 0 :memory tv :stopped? false :recur? true}) :output))
//
//(println answer)
//
//; 3780860499
//
//; part b
//(def answer-2 ((ic/op-code {:input 2 :output nil :phase nil :pointer 0 :relative-base 0 :memory tv :stopped? false :recur? true}) :output))
//
//(println answer-2)
//
//; 33343
