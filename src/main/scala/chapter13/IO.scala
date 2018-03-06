package chapter13

import chapter11.Monad

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F,B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this, f)
}
case class Return[F[_], A](a: A) extends Free[F,A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F,a]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(ma, f)
    }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(x) => x
    case Suspend(s) => s()
    case FlatMap(free, f) => free match {
      case Return(v) => runTrampoline { f(v) }
      case Suspend(s) => runTrampoline { f(s()) }
      case FlatMap(x, func) => runTrampoline { x.flatMap(func).flatMap(f)}
    }
  }

  private def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (value => f(value) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(x) => F.unit(x)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("not possible")
  }
}

