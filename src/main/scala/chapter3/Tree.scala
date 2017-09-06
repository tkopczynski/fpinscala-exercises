package chapter3

/**
  * Created by Tomasz Kopczynski.
  */
sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value:A) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + 1 + size(right)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => depth(left).max(depth(right)) + 1
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](t: Tree[A])(init: Leaf[A] => B)(f: Branch[A] => B): B = {
    t match {
      case Leaf(_) => init(t)
      case Branch(_,_) => f(t)
    }
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(b => sizeFold(b.left) + 1 + sizeFold(b.right))
  def depthFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(b => depthFold(b.left).max(depthFold(b.right)))
  def mapFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(l => Leaf(f(l.value)))(b => Branch(mapFold(b.left)(f), mapFold(b.right)(f)))

}