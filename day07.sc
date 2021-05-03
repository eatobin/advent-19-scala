import $file.intcode

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable

type Memory = TreeMap[Int, Int]

// part A
val memory: Memory = intcode.makeMemory("resources/day07.csv")

val shortMem = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0))
val myMem = intcode.makeShortMemory(Array(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 4))

val possibilities: Seq[TreeMap[Char, Int]] =
  for (a <- 0 to 4;
       b <- 0 to 4;
       c <- 0 to 4;
       d <- 0 to 4;
       e <- 0 to 4
       if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap

def pass(possible: TreeMap[Char, Int])(memory: Memory): Int = {
  intcode.IntCode.opCode(intcode.IntCode(
    input = intcode.IntCode.opCode(intcode.IntCode(
      input = intcode.IntCode.opCode(intcode.IntCode(
        input = intcode.IntCode.opCode(intcode.IntCode(
          input = intcode.IntCode.opCode(intcode.IntCode(
            input = 0,
            output = 0,
            phase = possible('a'),
            pointer = 0,
            relativeBase = 0,
            memory = memory,
            stopped = false,
            recur = true)).output,
          output = 0,
          phase = possible('b'),
          pointer = 0,
          relativeBase = 0,
          memory = memory,
          stopped = false,
          recur = true)).output,
        output = 0,
        phase = possible('c'),
        pointer = 0,
        relativeBase = 0,
        memory = memory,
        stopped = false,
        recur = true)).output,
      output = 0,
      phase = possible('d'),
      pointer = 0,
      relativeBase = 0,
      memory = memory,
      stopped = false,
      recur = true)).output,
    output = 0,
    phase = possible('e'),
    pointer = 0,
    relativeBase = 0,
    memory = memory,
    stopped = false,
    recur = true)).output
}

def passes(memory: Memory): Seq[Int] = possibilities.map(pass(_)(memory))

val answer: Int = passes(memory).max

println(s"Answer Part A: $answer")

// Answer Part A: 368584

// part B
val possibilities2: Seq[TreeMap[Char, Int]] =
  for (a <- 5 to 9;
       b <- 5 to 9;
       c <- 5 to 9;
       d <- 5 to 9;
       e <- 5 to 9
       if List(a, b, c, d, e).distinct.size == List(a, b, c, d, e).size)
  yield TreeMap[Char, Int]() ++ (List('a', 'b', 'c', 'd', 'e') zip List(a, b, c, d, e)).toMap

def makeAnAmpPass(possibility: TreeMap[Char, Int])(memory: Memory): mutable.Map[Int, intcode.IntCode] = {
  val fiveAmps: mutable.Map[Int, intcode.IntCode] = mutable.Map(
    1 -> intcode.IntCode(input = 0, output = 0, phase = possibility('a'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    2 -> intcode.IntCode(input = 0, output = 0, phase = possibility('b'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    3 -> intcode.IntCode(input = 0, output = 0, phase = possibility('c'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    4 -> intcode.IntCode(input = 0, output = 0, phase = possibility('d'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false),
    5 -> intcode.IntCode(input = 0, output = 0, phase = possibility('e'), pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = false)
  )
  fiveAmps
}

def toAmpsList(possibilitiesList: Seq[TreeMap[Char, Int]])(memory: Memory): Seq[mutable.Map[Int, intcode.IntCode]] = {
  possibilitiesList.map(makeAnAmpPass(_)(memory))
}

def runner(fiveAmps: mutable.Map[Int, intcode.IntCode]): Int = {
  @tailrec
  def recur(amps: mutable.Map[Int, intcode.IntCode], currentAmpNo: Int): Int = {
    val nextAmpNo: Int = (currentAmpNo % 5) + 1
    if (currentAmpNo == 5 && amps(currentAmpNo).stopped) {
      amps(currentAmpNo).output
    } else {
      amps(currentAmpNo) = intcode.IntCode.opCode(amps(currentAmpNo))
      amps(nextAmpNo) = amps(nextAmpNo).copy(input = amps(currentAmpNo).output)
      recur()
    }
  }

  recur(amps = fiveAmps, currentAmpNo = 1)
}

//(defn runner [five-amps]
//  (loop [amps five-amps
//    current-amp-no 1
//next-amp-no (+ 1 (mod current-amp-no 5))]
//(if (and (= 5 current-amp-no) (:stopped? @(amps current-amp-no)))
//(:output @(amps current-amp-no))
//(do (swap! (amps current-amp-no) ic/op-code)
//  (swap! (amps next-amp-no) assoc :input (:output @(amps current-amp-no)))
//(recur
//(assoc amps current-amp-no (amps current-amp-no) next-amp-no (amps next-amp-no))
//  next-amp-no
//(+ 1 (mod next-amp-no 5)))))))

//val ic2: intcode.IntCode = intcode.IntCode.opCode(intcode.IntCode(input = 5, output = 0, phase = 0, pointer = 0, relativeBase = 0, memory = memory, stopped = false, recur = true))
//val answer2: Int = ic2.output
//println(s"Answer Part B: $answer2")

// Answer Part B: 11981754

//  amp1 = amp1.copy(phase = possibility('a'), memory = memory)
//  amp2 = amp2.copy(phase = possibility('b'), memory = memory)
//  amp3 = amp3.copy(phase = possibility('c'), memory = memory)
//  amp4 = amp4.copy(phase = possibility('d'), memory = memory)
//  amp5 = amp5.copy(phase = possibility('e'), memory = memory)
