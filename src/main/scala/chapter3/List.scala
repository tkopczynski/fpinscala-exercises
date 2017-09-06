package chapter3

/**
  * Created by Tomasz Kopczynski.
  */

sealed trait List[+A]
case class Cons[+A](head:A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](h: A, as: List[A]): List[A] = {
    as match {
      case Nil => Cons(h, Nil)
      case Cons(_, tail) => Cons(h, tail)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) drop(tail(l), n - 1)
    else l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, tail) => {
        if (f(h)) dropWhile(tail, f)
        else l
      }
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f( x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,b) => 1 + b)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((b, _) => 1 + b)

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
  }

  def append[A](as: List[A], e: A): List[A] = {
    foldLeft(reverse(as), Cons(e, Nil))((accumulated, current) => Cons(current, accumulated))
  }

  def appendList[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l2, l1)((l, e) => append(l, e))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldLeft(as, Nil:List[A])((b: List[A], a:List[A]) => appendList(b, a))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil:List[B])((l, e) => append(l, f(e)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, Nil:List[A])((l, e) => {
      if (f(e)) {
        append(l, e)
      }
      else {
        l
      }
    })
  }

  def filterFlat[A](as: List[A])(f: A => Boolean) : List[A] = {
    flatMap(as)(elem => {
      if (f(elem)) {
        List(elem)
      } else {
        Nil
      }
    })
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil:List[B])((l,e) => appendList(l, f(e)))
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = {
    l1 match {
      case Nil => Nil
      case Cons(head, tail) => {
        l2 match {
          case Nil => Nil
          case Cons(head2, tail2) => Cons(f(head, head2), zipWith(tail, tail2)(f))
        }
      }
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasSubsequenceRecursive[A](supCurrent: List[A], subCurrent: List[A]): Boolean = {
      subCurrent match {
        case Nil => true
        case Cons(subHead, subTail) => {
          supCurrent match {
            case Nil => false
            case Cons(supHead, supTail) => {
              if (subHead == supHead) {
                hasSubsequenceRecursive(supTail, subTail)
              } else {
                hasSubsequence(supTail, sub)
              }
            }
          }
        }
      }
    }
    hasSubsequenceRecursive(sup, sub)
  }
}