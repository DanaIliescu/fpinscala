// // Advanced Programming 2015
// // Andrzej Wasowski, IT University of Copenhagen
// package fpinscala.monoids
// import scala.language.higherKinds

// // MONOID HAS OP (NEEDS TO BE ASSOCIATIVE) AND ZERO (ELEMENT NEUTRU / IDENTITY ELEMENT)
// trait Monoid[A] {

//   def op(a1: A, a2: A): A
//   def zero: A

// }

// object Monoid {

//   val stringMonoid = new Monoid[String] {
//     def op(a1: String, a2: String) = a1 + a2
//     val zero = ""
//   }

//   // I guess such objects could be useful if we combine them with implicits

//   def listMonoid[A] = new Monoid[List[A]] {
//     def op (a1: List[A], a2: List[A]) = a1 ++ a2
//     val zero = Nil
//   }

//   // Exercise 1
//   // Give Monoid instances for integer addition, multiplication, and for Boolean operators.
//   val intAddition = new Monoid[Int] {
//     def op(a: Int, b: Int) = a + b
//     val zero = 0
//   }
//   val intMultiplication = new Monoid[Int] {
//     def op(a: Int, b: Int) = a * b
//     val zero = 1
//   }
//   val booleanOr = new Monoid[Boolean] {
//     def op(a1: Boolean, a2: Boolean) = a1 || a2
//     val zero = false
//   }
//   val booleanAnd = new Monoid[Boolean] {
//     def op(a1: Boolean, a2: Boolean) = a1 && a2
//     val zero = true
//   }

//   // Exercise 2
//   // Give a Monoid instance for combing Option values.
//   // The composition operator should return its left argument if its not None,
//   // otherwise it should return the right argument.
//   def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
//     def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
//     val zero = None
//   }
//   def optionMonoid[A] = new Monoid[Option[A]] {
// 	   def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1 != None) a1 else a2
// 	   val zero = None
//   }

//   // We can get the dual of any monoid just by flipping the `op`.
//   def dual[A] (m :Monoid[A]) = new Monoid[A] {
//     def op (a1: A, a2: A) = m.op(a2,a1)
//     val zero = m.zero
//   }

//   // Now we can have both monoids on hand:
//   def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
//   def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

//   // Exercise 3
//   // A function having the same argument and return type is sometimes called an
//   // endofunction. Write a monoid for endofunctions.
     // #question
//   def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
//     def op(f: A => A, g: A => A) = f compose g
//     val zero = identity(_: A)
//   }

    // def endoMonoid[A]: Monoid[A =>A] = new Monoid[A =>A] {
    //   def op(a1: A => A, a2: A => A): A => A = a1 andThen(a2)
    //   val zero = (a: A) => a
    // }

//   // Exercise 4 is solved in MonoidSpec.scala

//   def concatenate[A] (as: List[A], m: Monoid[A]): A =
//     as.foldLeft (m.zero) (m.op)

//   // Exercise 7
//   //
//   // Implement a productMonoid that builds a monoid out of two monoids. Test it
//   // with scala check for instance by composing an Option[Int] monoid with a
//   // List[String] monoid and running through our monoid laws.

//   def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) : Monoid[(A, B)] = new Monoid[(A, B)] {
//     def op(a1: (A, B), a2: (A,B)) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))
//     val zero = (ma.zero, mb.zero)
//   }

// }


// trait Foldable[F[_]] {

//   def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
//   def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
//   def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
//   def concatenate[A](as: F[A])(m: Monoid[A]): A =
//     foldLeft(as)(m.zero)(m.op)

//   // Exercise 9 (CB 10.15)
//   // Any Foldable structure can be turned into a List. Write this conversion in a generic way.

//   def toList[A] (fa: F[A]) :List[A] =
//     foldLeft(fa)(List[A]())((b: List[A], a :A) => a::b)
// }

// // Exercise 8 (CB 10.12 We just do Foldable[List])

// object Foldable extends Foldable[List] {

//   def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B = as match {
//     case Nil => b
//     case h::t => f(h, foldRight (t) (b) (f))
//   }

//   def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B = as match {
//     case Nil => b
//     case h::t => foldLeft (t) (f(b, h)) (f)
//   }

//   def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B =
//     foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))
// }

// // vim:cc=80:tw=80
