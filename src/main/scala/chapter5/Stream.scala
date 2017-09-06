package chapter5

/**
  * Created by Tomasz Kopczynski.
  */
sealed trait Stream[+A] {
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(hd, tl) => hd() :: tl().toList
    }

  def take(n: Int): List[A] = {
    if (n > 0) {
      this match {
        case Empty => Nil
        case Cons(hd, tl) => hd() :: tl().take(n-1)
      }
    } else Nil
  }

  def drop(n: Int): List[A] = {
      this match {
        case Empty => Nil
        case Cons(hd, tl) => {
          if (n > 0) tl().drop(n-1)
          else hd() :: tl().drop(n-1)
        }
      }
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
