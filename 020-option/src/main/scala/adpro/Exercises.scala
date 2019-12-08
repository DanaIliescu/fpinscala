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
// This file is compiled with 'sbt compile' and tested with 'sbt test'.
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

/* We create OrderedPoint as a trait instead of a class, so we can mix it into
 * Points (this allows to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, We would have to
 * reimplement them in the subclass, if classes not traits are used.  This is
 * not a problem if I mix in a trait construction time. */

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>
  // Implement the missing method compare from Ordered[Point], using the lexicographic ordering,
  // i.e. (x, y) < (x,y) iff x<x o rx=x ∧ y<y.
  override def compare (that: java.awt.Point): Int = {
    if (this.x < that.x || (this.x == that.x && this.y < that.y)) -1
    else if (this.x == that.x && this.y == that.y) 0
    else 1
  }

}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

// Chapter 3


sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Write a function size that counts nodes (leaves and branches) in a tree.
  def size[A] (t :Tree[A]): Int =
    t match {
      case Branch(left, right) => size(left) + size(right) + 1
      case Leaf(value) => 1
    }

  // Exercise 3 (3.26)
  // Write a function maximum that returns the maximum element in a Tree[Int].
  def maximum (t: Tree[Int]): Int = {
    def go(m: Int): Int =
      t match {
        case Branch(left, right) => maximum(left) max maximum(right)
        case Leaf(value) => value max m
      }
    go(-2147483648)
  }

  // Exercise 4 (3.28)
  // Write a function map, analogous to the function of the same name on List.
  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] =
    t match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }

  // Exercise 5 (3.29)
  // Generalize size, maximum, and map, writing a new function fold that abstracts over their similarities.
  // Reimplement them.
  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B =
    t match {
      case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g))
      case Leaf(value) => g(value)
    }

  def size1[A] (t: Tree[A]): Int = fold(t)((acc:Int, curr:Int) => acc + curr + 1)(_ => 1)

  def maximum1[A] (t: Tree[Int]): Int = fold(t)((acc:Int, curr:Int) => acc max curr)((a:Int) => a)

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] =
    fold(t)((acc: Tree[B], curr: Tree[B]) => Branch(acc, curr): Tree[B])(a => Leaf(f(a)): Tree[B])

}

sealed trait Option[+A] {

  // Exercise 6 (4.1)
  // Implement map, getOrElse, flatMap, filter on Option.
  def map[A,B] (f: A=>B): Option[B] = this match {
    case None => None
    case Some(a: A) => Some(f(a))
  }

  // You may Ignore the arrow in default's type below for the time being.
  // (it should work (almost) as if it was not there)
  // It prevents the argument "default" from being evaluated until it is needed.
  // So it is not evaluated in case of Some (the term is 'call-by-name' and we
  // should talk about this soon).

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(b) => b
  }

  def flatMap[B] (f: A=>Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def filter (p: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (p(a)) this else None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7 (4.2)
  // Implement the variance function in terms of flatMap.
  def variance (xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // Exercise 8 (4.3)
  // Write a generic function map2 that combines two Option values using a binary function.
  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] =
    ao flatMap (a => bo map (b => f(a, b)))

  // Exercise 9 (4.4)
  // Write a function sequence that combines a list of Options into one Option
  // containing a list of all the Some values in the original list.
  def sequence[A] (aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight[Option[List[A]]](Some(Nil))((o, acc) => map2(o, acc)( (a, b) => a :: b))

  // Exercise 10 (4.5)
  // The function behaves like map executed sequentially on the list a, where the mapped function f can fail.
  // If at least one application fails, then the entire computation of the mapping (traversal) fails.
  def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] =
    sequence(as map (f))

  //   def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] = as match {
  //       case Nil => Some(Nil)
  //       case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  //     }

}
