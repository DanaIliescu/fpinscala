// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

// Group number: 23
// AUTHOR1: Dana-Maria Iliescu
// AUTHOR2: Sara Qirko
// AUTHOR3: Petr Sindelar

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
      // if f requires to go deeply into the stream. So folds sometimes may
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


  //Exercise 2
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }

  //Exercise 3
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
  
  //Exercise 4
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  //Exercise 5
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)


  //Exercise 6
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)

  //Exercise 7
  def headOption2 () :Option[A] = 
    foldRight(None: Option[A])((h,_) => Some(h))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))
    
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  //Exercise 09
  //Put your answer here:
  //With streams, as soon as this.filter(p) finds a match, it returns it and the find method terminates
  //With lists, this.filter(p) would loop through the whole list, create an intermediate list with the matches found, and then return the first value

  //Exercise 13
  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }
  def take2 (n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h,t), 1) => Some((h(), (empty, 0)))
    case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
    case _ => None
  }
  def takeWhile3 (f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if f(h()) => Some((h(), t()))
    case _ => None
  }
  def zipWith [B,C](f: (A,B) => C)(s2: Stream[B]): Stream[C] = unfold((this, s2)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
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


  //Exercise 1
  def from(n:Int):Stream[Int]=cons(n,from(n+1))

  def to(n:Int):Stream[Int]= cons(n,from(n-1))

  val naturals: Stream[Int] = from(0)


  //Exercise 10
  val fibs = {
    def go(a:Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }
    go(0, 1)
  }

  //Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
  f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  //Exercise 12
  def fib2 = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  def from2 (n: Int) = unfold(n)(n => Some((n,n+1)))

}

