// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package adpro

sealed trait Stream[+A] {
  import Stream._

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  // Exercise 2
  // Write a function to convert a Stream to a List, which will force its evaluation.
  def toList: List[A] = foldRight[List[A]](List())((a, acc) => a :: acc)

  // Exercise 3
  // Write the function take(n) for returning the first n elements of a Stream, and drop(n)
  // for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
  // {
  //   if(n > this.toList.length) sys.error("n is more than length of the stream")
  //   else if (n > 0) this match {
  //     case Empty => Empty
  //     case Cons(head,tail) => tail().drop(n - 1)
  //   } else this
  // }
  
  // Exercise 4
  // Write the function takeWhile (p) for returning all starting elements of a Stream that match the given predicate p.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }


  // Exercise 5
  // Implement forAll (p), which checks that all elements in this Stream satisfy a given
  // predicate. Terminate the traversal as soon as it encounters a non-matching value.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true) {(a, acc) =>
      if(!p(a)) return false
      p(a) && acc
    }


  // Exercise 6
  // Use foldRight to implement takeWhile.
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,acc) => if (p(h)) cons(h,acc) else empty)

//    FROM THE BOOK
//   //Exercise 5
//   def forAll(p: A => Boolean): Boolean = 
//     foldRight(true)((a,b) => p(a) && b)


//   //Exercise 6
//   def takeWhile2(p: A => Boolean): Stream[A] =
//     foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)

  // Exercise 7
  // Implement headOption using foldRight.
  def headOption2 () :Option[A] =
    foldRight[Option[A]](None)((h,acc) => Some(h))

  // Exercise 8 The types of these functions are omitted as they are a part of the exercise.
  // Implement map, filter, append, and flatMap using foldRight.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,acc) => cons(f(h), acc)) // !!! def map[B] !!! empty[B]
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h,acc) => if(p(h)) cons(h, acc) else acc)
  def append[B>:A](other: => Stream[B]): Stream[B] = foldRight(other)((h,acc) => cons(h, acc))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,acc) => f(h).append(acc))

  // Exercise 09
  // The book presents the following implementation for find:
  // def find (p :A => Boolean) :Option[A]= this.filter(p).headOption
  // Explain why this implementation is suitable (efficient) for streams and would not be optimal for lists.
  // Put your answer here:
  // With streams, as soon as this.filter(p) finds a match, it returns it and the find method terminates
  // With lists, this.filter(p) would loop through the whole list, create an intermediate list with the matches found, and then return the first value

  // Exercise 10
  // Compute a lazy stream of Fibonacci numbers fibs: 0, 1, 1, 2, 3, 5, 8, and so on. It can be done with functions available so far.
  // Test it be translating to List a finite prefix of fibs, or a finite prefix of an infinite suffix.
  // Put your answer here:
  def fib: Stream[Int] = {
    def go (a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }
    go(0, 1)
  }

  // Exercise 11
  // Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }


  // Exercise 12
  // Write fib and from in terms of unfold.
  def fib2: Stream[Int] = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  def from2(n: Int): Stream[Int] = unfold(n) (n => Some((n, n + 1)))

  // Exercise 13
  // Use unfold to implement map, take, takeWhile, and zipWith.
  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }
  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
    case (Cons(h,t), 1) => Some((h(), (empty, 0)))
    case _ => None
  }
  def takeWhile3(p: A => Boolean): Stream[A] = unfold((this, p)) {
    case (Cons(h, t), p) if p(h()) => Some(h(), (t(), p))
    case _ => None
  }
  def zipWith[A,B,C] (f: (A,B)=>C) (l: Stream[A], r: Stream[B]): Stream[C] =
    unfold((l, r)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1
  // Define functions from and to that generate streams of natural numbers above (and below) a given natural number.
  def from(n:Int):Stream[Int] = cons(n,from(n+1))

  def to(n:Int):Stream[Int] = cons(n,from(n-1))

  val naturals: Stream[Int] = from(0)


}

