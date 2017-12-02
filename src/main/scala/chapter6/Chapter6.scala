package chapter6

import chapter6.RNG.Simple

/**
  * Created by Tomasz Kopczynski.
  */
object Chapter6 {
  def main(args: Array[String]): Unit = {
    println(RNG.doubleRand(Simple(1L))._1)
  }
}
