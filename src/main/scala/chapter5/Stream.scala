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

  def take(n: Int): Stream[A] = {
    if (n > 0) {
      this match {
        case Empty => Empty
        case Cons(hd, tl) => Cons(hd,() => tl().take(n-1))
      }
    } else Empty
  }

  def drop(n: Int): Stream[A] = {
      this match {
        case Cons(_, tl) if n > 0 => tl().drop(n-1)
        case _ => this
      }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile_fold(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) Stream.cons(a, b) else Empty)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((a,_) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => Stream.cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => f(h).append(t))

  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) { case Cons(h1, t1) => Some(f(h1()), t1()) }

  def takeUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), n1) if n1 > 0 => Some((h(), (t(), n1-1)))
      case (Empty, _) => None
      case (_, 0) => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    this
      .zipAll(s)
        .forAll {
          case (Some(e1), Some(e2)) if e1 == e2 => true
          case _ => false
        }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(s => s.foldRight(z)(f))
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] =
      cons(n2, go(n2, n1 + n2))

    go(0, 1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Stream.empty
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }

  val fibsUnfold: Stream[Int] = {
    unfold((0,1)) { case (a,b) => Some((a, (b, a+b))) }
  }

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s+1))

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val onesUnfold: Stream[Int] =
    unfold(1)(_ => Some(1,1))

  def zipWith[A](l1: Stream[A], l2: Stream[A])(f: (A,A) => A): Stream[A] =
    Stream.unfold((l1, l2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

}
