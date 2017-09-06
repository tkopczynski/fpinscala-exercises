package chapter4

import chapter3.Cons
import chapter3.List
import chapter3.Nil

/**
  * Created by Tomasz Kopczynski.
  */
class Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .flatMap(m =>
        Some(
          xs
            .map(x => math.pow(x - m, 2))))
      .flatMap(s => mean(s))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap(a => b.map(b => f(a,b)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def seq[A](l: List[Option[A]], acc: List[A]): Option[List[A]] = {
      l match {
        case Nil => Some(acc)
        case Cons(head, tail) => {
          head match {
            case None => None
            case Some(v) => seq(tail, Cons(v, acc))
          }
        }
      }
    }

    seq(a, List())
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def t[A](l: List[A], acc: List[B]): Option[List[B]] = {
      l match {
        case Nil => Some(acc)
        case Cons(head, tail) => {
          f(head) match {
            case None => None
            case Some(v) => t(tail, Cons(v, acc))
          }
        }
      }
    }

    t(a, List())
  }

  def sequence_with_traverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a => a)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = {
    traverse_either(es)(a => a)
  }

  def traverse_either[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    def t(l: List[A], acc: List[B]): Either[E, List[B]] = {
      l match {
        case Nil => Right(acc)
        case Cons(head, tail) => {
          f(head) match {
            case Left(e) => Left(e)
            case Right(v) => t(tail, Cons(v, acc))
          }
        }
      }
    }
    t(as, List())
  }
}
