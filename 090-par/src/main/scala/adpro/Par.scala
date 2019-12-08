package adpro
import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

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
  // Use lazyUnit to write a function that converts any function A => B to one that evaluates its result
  // asynchronously (so it spawns a separate thread).
  def asyncF[A,B] (f: A => B) : A => Par[B] = a => unit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)
  // Write a function sequence that takes a list of parallel computations (List[Par[B]]) and
  // returns a parallel computation producing a list (Par[List[B]]).
  def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft[Par[List[A]]](unit(Nil))((acc,a) => map2(a,acc)(_ :: _))

  // Exercise 3 (CB7.6)

  // this is shown in the book:

  // def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
  //   val fbs: List[Par[B]] = ps.map(asyncF(f))
  //   sequence(fbs)
  // }
  
  // Implement parFilter, which filters elements of a list in parallel
  // (so the predicate f is executed in parallel for the various lists elements).
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldLeft[Par[List[A]]](unit(Nil))((acc,a) => if(f(a)) map2(unit(a),acc)(_ :: _) else acc)

  // Exercise 4
  // Implement map3 using map2.
  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) :Par[D] =
    map2(pa,map2(pb,pc){(b,c) => (b,c)}){(a,bc) => {
      val (b,c) = bc
      f(a,b,c)
    }}

  // shown in the book

  // def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)
  // Implement choiceN and then choice in terms of choiceN.
  // Let's say that choiceN runs n, and then uses that to select a parallel computation from choices.

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    es => {
      val nth = run(es)(n).get
      choices(nth)(es)
    }

  def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    choiceN(unit(2))(List(t,f))

  // Exercise 6 (CB7.13)
  // Implement a general parallel computation chooser, and then use it to implement choice and choiceN.
  // A chooser uses a parallel computation to obtain a selector for one
  // of the available parallel computations in the range provided by choices:
  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get()
      choices(a)(es)
    }

  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    chooser(n)(choices)

  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    chooser(cond)({b: Boolean => if(b) t else f})
  // ATENTIE LA EROAREA DE MAI JOS
  // found   : List[java.util.concurrent.ExecutorService => java.util.concurrent.Future[A]]
  // [error]  required: Boolean => adpro.Par.Par[A]
  //   chooser(cond)(List(t,f))

  // Exercise 7 (CB7.14)
  // Implement join. Can you see how to implement flatMap using join? And can you implement join using flatMap?
  def join[A] (a: Par[Par[A]]): Par[A] = chooser(a)(pa => pa)

  // def join[A] (a : Par[Par[A]]) :Par[A] = es => {
  //   val innerA = run(es) (a).get
  //   run (es) (innerA)
  // }

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}
