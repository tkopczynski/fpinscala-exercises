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
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n == as.length) true
      else if (! ordered(as(n - 1), as(n))) false
      else go(n + 1)
    }

    go(1)
  }

  // 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a,b) => f(a)(b)
  }

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(2,3,4), (a: Int, b: Int) => a <= b))
  }
}
