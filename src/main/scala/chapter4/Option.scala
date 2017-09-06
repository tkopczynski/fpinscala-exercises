package chapter4

/**
  * Created by Tomasz Kopczynski.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
      this match {
        case None => None
        case Some(get) => Some(f(get))
      }
}

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(get) => f(get)
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(get) => get
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(_) => this
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case Some(get) => {
        if (f(get)) Some(get)
        else None
      }
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
