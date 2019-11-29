package adpro
import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

// Group number: WorkGroup 23
//
// AUTHOR1: Dana-Maria Iliescu (dail@itu.dk)
// TIME1: 4,5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Sara Qirko (saqi@itu.dk)
// TIME2: 4,5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Petr Sindelar (pesi@itu.dk)
// TIME2: 4,5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // Exercise 3 (CB7.6)

  // this is shown in the book:

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fas: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(fas))(_.flatten)
  }

  // Exercise 4: implement map3 using map2

  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) :Par[D] = {
    val f1 = (pa: A, pb: B) => (pc: C) => f(pa, pb, pc)
    val f2: Par[C => D] = map2(pa, pb)(f1)
    map2(f2, pc)((f3: C => D, pc: C) => f3(pc))
  }

  // shown in the book

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    es => {
      val res = run(es)(n).get
      choices(res)(es)
    }

  def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    choiceN(unit(2))(List(t, f))

  // Exercise 6 (CB7.13)

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =
    es => {
      val res = run(es)(pa).get
      choices(res)(es)
    }

  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    chooser(n)(choices)

  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    chooser(cond)({b: Boolean => if (b) t else f})

  // Exercise 7 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] =
    es => {
      run(es)(run(es)(a).get())
    }

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}
