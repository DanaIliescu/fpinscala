// // Advanced Programming, A. Wąsowski, IT University of Copenhagen
// //
// // Group number: 23
// //
// // AUTHOR1: Sara Qirko
// // TIME1: 6 hours <- how much time have you used on solving this exercise set
// // (excluding reading the book, fetching pizza, and going out for a smoke)
// //
// // AUTHOR2: Dana-Maria Iliescu
// // TIME2: 3 hours <- how much time have you used on solving this exercise set
// // (excluding reading the book, fetching pizza, and going out for a smoke)
// //
// // AUTHOR3: Petr Sindelar
// // TIME3: 3 hours <- how much time have you used on solving this exercise set
// // (excluding reading the book, fetching pizza, and going out for a smoke)
// //
// // You should work with the file by following the associated exercise sheet
// // (available in PDF from the course website).
// //
// // This file is compiled with 'sbt compile' and tested with 'sbt test'.
// //
// // The file shall always compile and run after you are done with each exercise
// // (if you do them in order).  Please compile and test frequently. Of course,
// // some tests will be failing until you finish. Only hand in a solution that
// // compiles and where tests pass for all parts that you finished.    The tests
// // will fail for unfnished parts.  Comment such out.

// package adpro

// // Exercise  1

// /* We create OrderedPoint as a trait instead of a class, so we can mix it into
//  * Points (this allows to use java.awt.Point constructors without
//  * reimplementing them). As constructors are not inherited, We would have to
//  * reimplement them in the subclass, if classes not traits are used.  This is
//  * not a problem if I mix in a trait construction time. */

// trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

//   this: java.awt.Point =>

//   override def compare (that: java.awt.Point): Int =  {
//     if (that.x <= this.x && that.y<this.y) 1
//     else -1
//   }

// }

// // Try the following (and similar) tests in the repl (sbt console):
// // val p = new java.awt.Point(0,1) with OrderedPoint
// // val q = new java.awt.Point(0,2) with OrderedPoint
// // assert(p < q)

// // Chapter 3


// sealed trait Tree[+A]
// case class Leaf[A] (value: A) extends Tree[A]
// case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

// object Tree {

//   def size[A] (t :Tree[A]): Int = t match {
//     case Leaf(_) => 1
//     case Branch(l, r) => 1 + size(l) + size(r)  
//   }

//   // Exercise 3 (3.26)

//   def maximum (t: Tree[Int]): Int = t match {
//     case Leaf(n) => n
//     case Branch(l, r) => maximum(l) max maximum(r) 
//   }

//   // Exercise 4 (3.28)

//   def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
//     case Leaf(a) => Leaf(f(a))
//     case Branch(l, r) => Branch(map(l)(f), map(r)(f))
//   }

//   // Exercise 5 (3.29)

//   def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
//     case Leaf(a) => f(a)
//     case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
//   }

//   def size1[A] (t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

//   def maximum1[A] (t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

//   def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

// }

// sealed trait Option[+A] {

//   // Exercise 6 (4.1)

//   def map[B] (f: A=>B): Option[B] = this match {
//     case None => None
//     case Some(a) => Some(f(a))
//   }

//   // You may Ignore the arrow in default's type below for the time being.
//   // (it should work (almost) as if it was not there)
//   // It prevents the argument "default" from being evaluated until it is needed.
//   // So it is not evaluated in case of Some (the term is 'call-by-name' and we
//   // should talk about this soon).

//   def getOrElse[B >: A] (default: => B): B = this match {
//     case None => default
//     case Some(a) => a
//   }

//   def flatMap[B] (f: A=>Option[B]): Option[B] = this match {
//     case None => None
//     case Some(a) => f(a)
//   }

//   def filter (p: A => Boolean): Option[A] = this match {
//     case Some(a) if p(a) => this
//     case _ => None
//   }

// }

// case class Some[+A] (get: A) extends Option[A]
// case object None extends Option[Nothing]

// object ExercisesOption {

//   // Remember that mean is implemented in Chapter 4 of the text book

//   def mean(xs: Seq[Double]): Option[Double] =
//     if (xs.isEmpty) None
//     else Some(xs.sum / xs.length)

//   // Exercise 7 (4.2)

//   def variance (xs: Seq[Double]): Option[Double] = 
//     mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

//   // Exercise 8 (4.3)

//   def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = 
//     ao flatMap (a => bo map (b => f(a, b)))

//   // Exercise 9 (4.4)

//   def sequence[A] (aos: List[Option[A]]): Option[List[A]] = 
//     aos.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

//   // Exercise 10 (4.5)

//   def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] = as match {
//       case Nil => Some(Nil)
//       case h::t => map2(f(h), traverse(t)(f))(_ :: _)
//     }

// }
