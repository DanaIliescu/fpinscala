// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them.

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecWasowski extends FreeSpec with Matchers with PropertyChecks {

  import Stream._

  // A simple converter of lists to streams
  def list2stream[A] (la :List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // note that there is a name clash between Stream.empty and the testing
  // library, so we need to qualify Stream.empty

  // An example generator of random finite non-empty streams
  // (we use the built in generator of lists and convert them to streams,
  // using the above converter)
  //
  // 'suchThat' filters out the generated instances that do not satisfy the
  // predicate given in the right argument.
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  "headOption" - {

    // a scenario test:

    "returns None on an empty Stream (01)" in {
      (Stream.empty.headOption) shouldBe (None)
    }


    // two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {
      forAll { (n :Int) => cons (n, Stream.empty).headOption should be (Some (n)) }
    }

    "returns the head of random stream packaged in Some (02)" in {
      // The implict makes the generator available in the context
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // This property uses our generator of non empty streams thanks to the
      // above implicit declaration
      forAll { (s :Stream[Int]) => s.headOption shouldNot be (None) }
    }

    "doesn't force the tail of the Stream (03)" in {
      forAll { (n: Int) => cons(n, fail("tail is forced")).headOption should be (Some (n)) }
    }

  }

  "take" - {
    "does not force any heads nor any tails of the Stream it manipulates (04)" in {
      implicit def arbPositiveInt = Arbitrary[Int] (Gen.posNum[Int])
      forAll { (n: Int) => cons(fail("head is forced"), fail("tail is forced")).take(n).isInstanceOf[Stream[Int]] }
    }

    "does not force (n+1)st head ever (even if we force all elements of take(n)) (05)" in {
      implicit def arbPositiveInt = Arbitrary[Int] (Gen.posNum[Int])
      cons(1, cons(2, cons(fail("head is forced"), fail("tail is forced")))).take(2).toList should be (List(1,2))
    }

    "s.take(n).take(n) == s.take(n) for any Stream s and any n (idempotency) (06)" in {
      implicit def arbPositiveInt = Arbitrary[Int] (Gen.posNum[Int])
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      forAll { (n: Int, s: Stream[Int]) => s.take(n).take(n).toList should be (s.take(n).toList) }
    }
  }

  "drop" - {
    "s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity) (07)" in {
      implicit def arbPositiveInt = Arbitrary[Int] (Gen.posNum[Int])
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      forAll { (n: Int, m: Int, s: Stream[Int]) => s.drop(n).drop(m).toList should be (s.drop(n + m).toList) }
    }

    "does not force any of the dropped elements heads (08)" in {
      implicit def arbPositiveInt = Arbitrary[Int] (Gen.posNum[Int])
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (n: Int, s: Stream[Int]) =>
        cons(fail("dropped element is forced"), s).drop(n).isInstanceOf[Stream[Int]] }
    }

    "the above should hold even if we force some stuff in the tail (09)" in {
      implicit def arbPositiveInt = Arbitrary[Int] (Gen.posNum[Int])
      cons(fail("dropped head is forced"), cons(1, Stream.empty)).drop(1).toList should be (List(1))
    }
  }

  "map" - {
    "x.map(id) == x (where id is the identity function) (10)" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      forAll { (s: Stream[Int]) => s.map(identity).toList should be (s.toList) }
    }

    "terminates on infinite streams (11)" in {
      val naturals: Stream[Int] = Stream.from(1)
      naturals.map(identity)
      true
    }
  }

  "append" - {
      "s1.append(s2.append(s3)) == (s1.append(s2)).append(s3) for any streams s1, s2, s3 (associativity) (12)" in {
        implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
        forAll { (s1: Stream[Int], s2: Stream[Int], s3: Stream[Int]) =>
          s1.append(s2.append(s3)).toList should be ((s1.append(s2)).append(s3).toList) }
      }

      "s1.append(s2) == s1 if s2 is an empty stream (13)" in {
        implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
        forAll { (s1: Stream[Int]) => s1.append(Stream.empty).toList should be (s1.toList) }
      }

      "does not force any heads nor tails of the appended stream (13)" in {
        implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
        forAll { (s: Stream[Int]) => s.append(cons(fail("head is forced"), fail("tail is forced"))).isInstanceOf[Stream[Int]] }
      }
  }

}
