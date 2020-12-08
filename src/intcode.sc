import scala.collection.immutable.ListMap
import scala.io.{BufferedSource, Source}

def makeTV(file: String): ListMap[Int, Int] = {
  val bufferedSource: BufferedSource = Source.fromFile(file)
  val stringArray: Array[Int] = {
    bufferedSource
      .mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
  }
  val unSorted: Map[Int, Int] = Iterator.from(0).zip(stringArray).toMap
  ListMap(unSorted.toSeq.sortBy(_._1): _*)
}
