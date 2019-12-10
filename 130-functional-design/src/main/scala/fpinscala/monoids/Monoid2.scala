// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monoids
import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // I guess such objects could be useful if we combine them with implicits

  def listMonoid[A] = new Monoid[List[A]] {
    def op (a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 1

  val intAddition = new Monoid[Int] {
    def op(n1: Int, n2: Int): Int = n1 + n2
    val zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(n1: Int, n2: Int): Int = n1 * n2
    val zero = 1
  }
  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  // Exercise 2

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1 != None) a1 else a2
    val zero = None
  }

  def dual[A] (m :Monoid[A]) = new Monoid[A] {
    def op (a1: A, a2: A) = m.op(a2,a1)
    val zero = m.zero
  }

  // Exercise 3
  def endoMonoid[A]: Monoid[A =>A] = new Monoid[A =>A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen(a2)
    val zero = (a: A) => a
  }

  // Exercise 4 is solved in MonoidSpec.scala

  def concatenate[A] (as: List[A], m: Monoid[A]): A =
    as.foldLeft (m.zero) (m.op)

  // Exercise 7
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

  def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) = new Monoid[(A,B)] {
    def op(t1:(A,B), t2:(A,B)): (A,B) = (ma.op(t1._1, t2._1), mb.op(t1._2, t2._2))
    val zero = (ma.zero, mb.zero)
  }

}


trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 9 (CB 10.15)

  def toList[A] (fa: F[A]) :List[A] = foldLeft[A, List[A]](fa) (List()) ((acc, a) => a :: acc)
}

// Exercise 8 (CB 10.12 We just do Foldable[List])

object Foldable extends Foldable[List] {

  def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B = as match {
    case Nil => b
    case x :: xs => f (x, foldRight (xs) (b) (f))
  }

  def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B =
    foldRight(as) ((b:B) => b) ((a,g) => b => g(f(b,a)))(b)

  def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B =
    foldLeft(as) (mb.zero) ((acc, a) => mb.op(acc, f(a)))
}

// vim:cc=80:tw=80
