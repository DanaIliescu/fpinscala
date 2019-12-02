// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write ITU email addresses of both group members that contributed to
// the solution of the exercise (in lexicographic order).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain' command.
// To load the file int the REPL use the 'console' command.
// Now you can interactively experiment with your code.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests, after you are done with each
// exercise (if you do them in order).  Please compile and test frequently.

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.
package fpinscala
import scala.collection.immutable.Stream.Cons

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3
  // Write a recursive function to get the nth Fibonacci number.

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive
  def fib (n: Int) : Int = {
    @annotation.tailrec
    def loop (a: Int, b: Int, n: Int): Int = {
      if (n <= 1) a else loop(b, a + b, n - 1)
    }
    loop(0, 1, n)
  }

  // Exercise 4
  // Implement a function that checks if an Array[A] is sorted given a comparison function.

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive

  // NO PATTERN MATCHING ON ARRAYS!!!
  // def isSorted[A] (as: Array[A]) (ordered: (A,A) => Boolean) :Boolean = as match {
  //   case Nil => true
  //   case h::tail => if (ordered(h, tail.head)) isSorted(tail) (ordered) else false
  // }

  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean) :Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n == as.size - 1) true
      else if (ordered(as(n), as(n+1))) go(n + 1)
      else false
    }
    go(0)
  }

  // Exercise 5
  // Implement a currying function: a function that converts a function f of two argument
  // that takes a pair, into a function of one argument that partially applies f.
  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = {
    a => b => f(a,b)
  }

  val isSortedCurried = curry(isSorted)

  // Exercise 6
  // Implement uncurry, which reverses the transformation of curry.
  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = {
    (a, b) => f(a)(b)
  }

  // Use uncurry to obtain isSorted back from the curried version created in the Exercise 5.
  val isSortedUncurried = uncurry(isSortedCurried)

  // Exercise 7
  // Implement the higher-order function that composes two functions.

  def compose[A,B,C] (f: B => C, g: A => B) : A => C =
    a => f(g(a))

  // Exercise 8 requires no programming
  // What will be the result of the following match expression?
  // List(1,2,3,4,5) match {
  //   case Cons(x, Cons(2, Cons(4, _))) => x
  //   case Nil => 42
  //   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  //   case Cons(h, t) => h + sum(t)
  //   case _ => 101
  // }

  // Exercise 9
  // Implement the function tail for removing the first element of a list.
  def tail[A] (as: List[A]) :List[A] = as match {
    case Cons(head, tail) => tail
    case Nil => sys.error("list is empty")
  }

  // Exercise 10
  // Generalize tail to the function drop, which removes the first n elements from a list.
  // Note that this function takes time proportional only to the number of elements being dropped.
  @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = {
    if(n > length(l)) sys.error("n is more than length(l)")
    else if (n > 0) l match {
      case Nil => List()
      case Cons(head,tail) => drop[A](tail, n - 1)
    } else l
  }

  // Exercise 11
  // Implement dropWhile, which removes elements from the given list prefix as long
  // as they match a predicate f.
  @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => List()
    case Cons(head,tail) => if(f(head)) dropWhile[A](tail, f) else l
  }

  // Exercise 12
  // Implement a function, init, that returns a list consisting of all but the last element
  // of the original list. So, given List(1,2,3,4), init will return List(1,2,3).
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("list is empty")
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  // Exercise 13
  // Compute the length of a list using foldRight.
  def length[A] (as: List[A]): Int = foldRight(as, 0)((a, acc) => acc + 1)

  // Exercise 14
  // Write foldLeft, that is tail-recursive.
  @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match { 
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // Exercise 15
  // Write product (computing a product of a list of integers) and a function to compute
  // the length of a list using foldLeft.
  def product (as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def length1 (as: List[Int]): Int = foldLeft(as, 0)((acc, a) => acc + 1)

  // Exercise 16
  // Write a function that returns the reverse of a list (given List(1,2,3), it returns
  // List(3,2,1)). Use one of the fold functions (no recursion).
  def reverse[A] (as: List[A]): List[A] = foldLeft(as, List[A]())((acc, a) => Cons(a, acc))

  // Exercise 17
  // Write foldRight using foldLeft (Hint: use reverse).
  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = {
    val reversed = reverse[A](as)
    foldLeft(reversed, z)((b,a) => f(a,b))
  }

  // Exercise 18
  // Write foldLeft in terms of foldRight. Do not use reverse here.
  // Hint: to do this you will need to synthesize a function that computes the run of foldLeft,
  // and then invoke this function.
  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B =
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  // Exercise 19
  // Write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists.
  // Use append, which concatenates two lists.
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // NOTE!! Remember to add type A on empty List initial element.
  def concat[A] (as: List[List[A]]): List[A] = foldLeft(as, List[A]())(append)

  // Exercise MAP (3.18 from the book)
  // Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
  def map[A,B] (as: List[A]) (f: A => B): List[B] = foldLeft(as, List[B]())((acc, a) => append(acc, List(f(a))))

  // Exercise 20
  // Write a function filter that removes elements from a list unless they satisfy a given predicate f.
  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = as match {
    case Nil => List()
    case Cons(h, Nil) => if(f(h)) as else Nil
    case Cons(h, t) => if(f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  // Exercise 21
  // Write a function flatMap that works like map except that the function given will return
  // a list instead of a single result, and that list should be inserted into the final resulting list.
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 22
  // Use flatMap to implement filter.
  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] =
    flatMap(l)((a: A) => if (p(a)) List(a) else Nil)

  // Exercise 23
  // Write a function that accepts two lists and constructs a new list by adding corre-
  // sponding elements. For example, List(1,2,3) and List(4,5,6,7) become List(5,7,9).
  // Trailing elements of either list are dropped.
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
    case (Nil, Nil) => List()
    case (Nil, _) => sys.error("l is empty")
    case (_, Nil) => sys.error("r is empty")
    case (Cons(h1,Nil), Cons(h2,t2)) => Cons(h1 + h2, Nil)
    case (Cons(h1,t1), Cons(h2,Nil)) => Cons(h1 + h2, Nil)

    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, add(t1)(t2))
  }

  // Exercise 24
  // Generalize the function you just wrote so that it is not specific to integers or addition.
  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = (l,r) match {
    case (Nil, Nil) => List()
    case (Nil, _) => sys.error("l is empty")
    case (_, Nil) => sys.error("r is empty")
    case (Cons(h1,Nil), Cons(h2,t2)) => Cons(f(h1, h2), Nil)
    case (Cons(h1,t1), Cons(h2,Nil)) => Cons(f(h1, h2), Nil)

    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(f)(t1, t2))
  }

  // Exercise 25
  // Implement a function hasSubsequence for checking whether a List contains another List as a subsequence.
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

  // Exercise 26
  // Write a recursive function that generates the nth row of Pascal’s triangle.
  def pascal (n: Int): List[Int] = n match {
    case 1 => List(1)
    case 2 => List(1,1)
    case n: Int => concat(
      List(
        List(1),
        map( zipWith((a: Int, b: Int) => (a, b)) (pascal(n - 1), tail(pascal(n - 1))) ) ( { case (a, b) => a + b } ),
        List(1)
      )
    )
  }

}
