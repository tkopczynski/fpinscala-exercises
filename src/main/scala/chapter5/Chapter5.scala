package chapter5

/**
  * Created by Tomasz Kopczynski.
  */
object Chapter5 {

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3,4,5).drop(3))
//    println(Stream(1,2,3,4,5).take(3))
  }
}
