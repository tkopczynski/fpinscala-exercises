package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, rng2) = rng.nextInt
    if (nextInt == Int.MinValue) nonNegativeInt(rng2)
    else if (nextInt < 0) (nextInt * (-1), rng2)
    else (nextInt, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextDouble, rng2) = nonNegativeInt(rng)
    (nextDouble / Int.MaxValue.toDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intValue, rng2) = rng.nextInt
    val (doubleValue, rng3) = double(rng2)
    ((intValue, doubleValue), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intValue, doubleValue), rng2) = intDouble(rng)
    ((doubleValue, intValue), rng2)
  }

  def double3(rngBase: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double(rngBase)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (l, rng)
      } else {
        val (i, r) = rng.nextInt
        go(count - 1, r, i :: l)
      }
    }

    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - 1 % 2)

  def doubleRand: Rand[Double] =
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng3) = rb(rng1)
      (f(a, b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(fs1: List[Rand[A]], rng: RNG, xs: List[A]): (List[A], RNG) = {
      fs1 match {
        case Nil => (xs, rng)
        case h :: t => {
          val (random, rng1) = h(rng)
          go(t, rng1, random :: xs)
        }
      }
    }

    rng => go(fs, rng, List())
  }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def go(fs1: List[State[S, A]], s: S, xs: List[A]): List[A] = {
      fs1 match {
        case Nil => xs.reverse
        case h :: t => {
          val (xs1, s1) = h.run(s)
          go(t, s1, xs1 :: xs)
        }
      }
    }
    State(s => (go(fs, s, List()), s))
  }

  def sequenceFoldLeft[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    State(s => (fs.foldLeft((s: S, List(): List[A]))((acc, elem) => {
      val (a, s1) = elem.run(acc._1)
      (s1, a :: acc._2)
      }
    )._2, s))
  }

  def sequenceFoldLeft2[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List()))((acc, elem) => elem.map2(acc)(_ :: _))

  def sequenceFoldRight[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((elem, acc) => elem.map2(acc)(_ :: _))
}