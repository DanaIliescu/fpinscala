// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monads
import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A,B] (fa: F[A]) (f: A => B) :F[B]

  def distribute[A,B] (fab: F[(A,B)]): (F[A],F[B]) =
    (map (fab) (_._1), map (fab)(_._2))

  def codistribute[A,B] (e :Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map (fa) (Left(_))
    case Right(fb) => map (fb) (Right(_))
  }

}

// EVERY DATA TYPE THAT HAS A MAP FUNCTION IS A FUNCTOR
object Functor {

  val ListFunctor = new Functor[List] {
    def map[A,B] (as: List[A]) (f: A => B): List[B] = as.map (f)
  }

  // Exercise 10
  // Implement an instance of OptionFunctor.
  val OptionFunctor = new Functor[Option] {
    def map[A, B] (fa: Option[A]) (f: A => B): Option[B] = fa match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }
}

// EVERY DATA TYPE THAT HAS A MAP2 FUNCTION (+ MAP, UNIT, FLATMAP) IS A MONAD
trait Monad[F[_]] {

  def unit[A]  (a: => A): F[A]
  def flatMap[A,B] (ma: F[A]) (f: A => F[B]) :F[B]

  def map[A,B] (ma: F[A]) (f: A => B) :F[B] =
    flatMap (ma) (a => unit (f(a)))

  def map2[A, B, C] (ma: F[A], mb: F[B]) (f: (A,B) => C): F[C] =
    flatMap (ma) (a => map (mb) (b => f(a,b)))

  // Exercise 13 (CB11.3)
  // Implement sequence as a method of the Monad trait. Express it in terms of unit and map2.
  def sequence[A] (lfa: List[F[A]]): F[List[A]] =
    lfa.foldRight(unit(List[A]()))((ma, acc) => map2(ma, acc)(_ :: _))

  // traverse seems to simply sequence results of mapping.  I do not think that
  // it appeared in our part. You can uncomment it once you have sequence.
  // def traverse[A,B] (la: List[A]) (f: A => F[B]): F[List[B]] = sequence(la.map (f))

  // Exercise 14 (CB11.4)
  // Implement replicateM, which replicates a monad instance n times into an instance of a list monad.
  def replicateM[A] (n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
  // Explanation: Depending on what the type constructor F[_] is, 
  // our result will be either List[List[A]] or Option[List[A]], 
  // where List[A] becomes replicated n times.

  def join[A] (mma: F[F[A]]): F[A] = flatMap (mma) (ma => ma)

  // Exercise 15 is solved in MonadSpec.scala

  // Exercise 16 (CB11.7)
  // Implement the Kleisli composition function compose.
  def compose[A,B,C] (f: A => F[B], g: B => F[C]): A => F[C] = {
    (a :A) => flatMap(f(a))(g)
  }

}

object Monad {

  // Exercise 12 (CB11.1)
  // Write monad instances for Option and List.
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

}
