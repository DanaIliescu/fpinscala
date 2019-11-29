// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: 23
//
// AUTHOR1: Dana-Maria Iliescu, dail@itu.dk
// TIME1: 5-6 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Sara Qirko, saqi@itu.dk
// TIME2: 5-6 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Petr Sindelar, pesi@itu.dk
// TIME2: 5-6 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// This file is compiled with 'sbt compile' and tested with 'sbt test'.

package adpro

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG (seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG (newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt (rng: RNG): (Int, RNG) = {
		val (n, r) = rng.nextInt
		if (n < 0) (-(n + 1), r)
		else (n, r)
	}

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
		val ((i, d), r) = intDouble(rng)
		((d, i), r)
	}

  def boolean (rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int) (rng: RNG): (List[Int], RNG) = {
		if (count == 0)
			(List(), rng)
		else {
			val (x, r1) = rng.nextInt
			val (l, r2) = ints(count - 1)(r1)
			(x :: l, r2)
		}
	}

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5) (Lazy is added so that the class does not fail
  // at load-time without your implementation).

  lazy val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6 (CB 6.6)

  // the test for this exercise is failing...
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both(int, double)

  lazy val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
	}

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
		val (a, r1) = f(rng)
		g(a)(r1) 
	}

  def nonNegativeLessThan(n: Int): Rand[Int] = {
		flatMap(nonNegativeInt) { i =>
		val mod = i % n
		if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
		}
	}

}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  // the test for this exercise is failing...
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // This is given in the book:

  def modify[S](f: S => S): State[S, Unit] = for {
     s <- get // Gets the current state and assigns it to `s`.
     _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 10

  // We did not manage to find a solution for this exercise.
  // We hope to solve it in a exercise session with the TAs.
  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = ???
    
  // Exercise 11 (lazy is added so that the class does not crash at load time
  // before you provide an implementation).

  // We could not solve this exercise since we haven't solved exercise 10.
  lazy val random_integers = ???

}


// vim:cc=80:foldmethod=indent:nofoldenable
