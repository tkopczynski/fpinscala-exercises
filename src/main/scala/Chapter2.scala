/**
  * Created by Tomasz Kopczynski.
  */
object Chapter2 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(curr: Int, n: Int, prev1: Int, prev2: Int): Int = {
      if (curr == n) prev1 + prev2
      else go(curr + 1, n, prev2, prev1 + prev2)
    }

    if (n ==1) 0
    else if (n == 2) 1
    else go(3, n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n == as.length) true
      else if (! ordered(as(n - 1), as(n))) false
      else go(n + 1)
    }

    go(1)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(2,3,4), (a: Int, b: Int) => a <= b))
  }
}
