package chapter5

/**
  * Created by Tomasz Kopczynski.
  */
object Chapter5 {

  def main(args: Array[String]): Unit = {
//    println(Stream(1).tails.take(10).toList)
    println(Stream(1,2,3).scanRight(0)(_+_).toList)
//    println(Stream(1,2,3).map(_ * 2).toList)
//    println(Stream(1,2,3).headOption)
//    println(Stream(1,2,3,4,5).takeWhile_fold(_ < 4).toList)
//    println(Stream(1,2,3,4,5).forAll(_ % 2 == 0))
//    println(Stream(1,2,3,4,5).drop(3))
//    println(Stream(1,2,3,4,5).take(3))
  }
}
