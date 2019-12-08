// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).

package adpro.testing
import fpinscala.state._
import fpinscala.state.RNG._

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class Gen[A] (sample :State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))
  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 3
  // Implement a method Gen[A].listOfN, which given an integer number n returns a list of length n
  // containing A elements, generated by this generator.
  def listOfN (n: Int): Gen[List[A]] = Gen(State.sequence(List.fill(n)(this.sample)))

  // Exercise 4
  // Implement flatMap for generators. Recall that flatMap allows to run another generator on the result of the present one (this).
  // Note that in the type below the parameter A is implicitly bound, as this is meant to be a method of Gen[A].
  def flatMap[B] (f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap(a => f(a).sample))

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] = this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 5
  // Use flatMap to implement a more dynamic version of listOfN.
  def listOfN (size: Gen[Int]): Gen[List[A]] = {
    // size flatMap (n => this.listOfN(n))
    listOfN(size.sample.run(RNG.Simple(42))._1)
  }

  // Exercise 6
  // Implement union, for combining two generators of the same type into one, by pulling values from each generator with equal likelihood.
  def union (that: Gen[A]): Gen[A] = Gen.boolean flatMap (p => if (p) this else that)

  // Exercise 7 continues in the companion object (below)
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => n #:: state2stream (s) (s1) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  // Exercise 1
  // Implement a test case generator Gen.choose. It should generate integers in the range start to stopExclusive.
  def choose (start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // Exercise 2
  // Implement test case generators unit (always generates a constant value given to it in a parameter)
  def unit[A] (a: => A): Gen[A] = Gen(State(s => (a, s)))

  // boolean (generates randomly true, false)
  def boolean: Gen[Boolean] = Gen(State((rng: RNG) => RNG.boolean(rng)))

  // double (generates random double numbers)
  def double: Gen[Double] = Gen(State((rng: RNG) => RNG.double(rng)))

  // (Exercise 3 is found in the Gen class above)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result { def isFalsified: Boolean }
  case object Passed extends Result { def isFalsified = false }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
      def isFalsified = true
  }
  case object Proved extends Result { def isFalsified = false }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => as.toStream(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop (run: (TestCases,RNG)=>Result) {

  // (Exercise 7)
  // Implement Prop[A].&& and Prop[A].|| for composing Prop values.
  // The former should succeed only if both composed properties (this and that) succeed;
  // the latter should fail only if both composed properties fail.
  def && (that: Prop): Prop = Prop {
    (testCases, rng) =>
      val res = this.run(testCases, rng)
      if (res.isFalsified) res
      else that.run(testCases, rng)
  }

  def || (that: Prop): Prop = Prop {
    (testCases, rng) =>
      val res = this.run(testCases, rng)
      if (res.isFalsified) that.run(testCases, rng)
      else res
  }

}

// vim:cc=80:foldmethod=indent:nofoldenable
