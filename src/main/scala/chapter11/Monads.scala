package chapter11

import chapter4.{Option, Some}
import chapter5.Stream
import chapter6.State
import chapter7.Par
import chapter7.Par.Par

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    def go(l: List[F[A]], acc: F[List[A]]): F[List[A]] = {
      l match {
        case Nil => map(acc)(_.reverse)
        case h :: t => go(t, map2(acc, h)((a, e) => e :: a))
      }
    }
    go(lma, unit(scala.collection.immutable.List()))
  }

  def sequenceWithFold[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]()))((fa: F[A], acc: F[List[A]]) => map2(fa, acc)(_ :: _))
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    def go(i: Int, acc: F[List[A]]): F[List[A]] = {
      if (i > 0) {
        go(i -1, map2(ma, acc)(_ :: _))
      } else {
        acc
      }
    }
    go(n, unit(List[A]()))
  }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]()))((elem, acc) => {
      val filtered = f(elem)
      map2(filtered, acc)((filteredValue, list) => {
        if (filteredValue) {
          elem :: list
        } else {
          list
        }
      })
    })
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapCompose[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def flatMapJoin[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

}


object Monads {

  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[chapter3.List] {
    override def unit[A](a: => A): chapter3.List[A] = chapter3.List(a)
    override def flatMap[A, B](ma: chapter3.List[A])(f: A => chapter3.List[B]): chapter3.List[B] = chapter3.List.flatMap(ma)(f)
  }

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val stateMonad = new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = State.unit(a)
      override def flatMap[A, B](ma: StateS[A])(f: A => StateS[B]): StateS[B] = ma.flatMap(f)
    }
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(R => a)
    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
  }
}
