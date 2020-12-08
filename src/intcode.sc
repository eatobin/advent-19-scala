import scala.collection.SortedMap
import scala.io.{BufferedSource, Source}

def makeTV(file: String): SortedMap[Int, Int] = {
  val bufferedSource: BufferedSource = Source.fromFile(file)
  val stringArray: Array[Int] = {
    bufferedSource
      .mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  }
  val unSortedMap: Map[Int, Int] = Iterator.from(0).zip(stringArray).toMap
  SortedMap[Int, Int]() ++ unSortedMap
}
