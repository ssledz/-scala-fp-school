package fpdesign

import basics.homework2.Monoid
import fpdesign.functors.Functor
import fpdesign.functors.Functor.instances._

/**
  *  To Read:
  *    - https://en.wikipedia.org/wiki/Type_constructor
  *    - https://en.wikipedia.org/wiki/Bottom_type
  *    - https://en.wikipedia.org/wiki/Type_theory
  *    - https://github.com/typelevel/kind-projector
  *    - https://en.wikipedia.org/wiki/Functor
  *    - https://en.wikipedia.org/wiki/Natural_transformation
  *    - https://en.wikipedia.org/wiki/Endomorphism
  *
  *  Try to implement:
  *    - functor instance for
  *      - Read
  *      - Show
  *      - Either
  *      - Hash
  *    - what are your observations (if any) ?
  *    - implement parse
  *      - is it safe ?
  *    - implement foldMap, Foldable, intMonoidInstance, vectorFolableInstance, vectorFunctorInstance to assert a given expression
  *    - implement distribute and codistribute
  */
object homework4 extends App {

  trait Read[A] {
    def read(s: String): A
  }

  trait Show[A] {
    def show(a: A): String
  }

  trait Hash[A] {
    def hash(x: A): Int
  }

  def parse[F[_]: Functor](xs: F[String]): F[Int] = ???

  implicit val setFunctorInstance: Functor[Set] = ???

  assert(parse(Set("1", "2", "3")) == Set(1, 2, 3))

  trait Foldable[F[_]] {
    ???
  }

  def foldMap[F[_]: Functor: Foldable, A, B: Monoid](fa: F[A])(f: A => B): B = ???

  case class Money(value: Int)
  implicit val intMonoidInstance: Monoid[Int] = ???
  implicit val vectorFunctorInstance: Functor[Vector] = ???
  implicit val vectorFolableInstance: Foldable[Vector] = ???

  assert(foldMap(Vector(Money(1), Money(2), Money(3)))(_.value) == 6)

  def distribute[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = ???

  def codistribute[F[_]: Functor, A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = ???

  assert(distribute(List("one" -> 1, "two" -> 2, "three" -> 3)) == (List("one", "two", "three"), List(1, 2, 3)))
  assert(codistribute(Right(List(1, 2, 3))) == List(Right(1), Right(2), Right(3)))
  assert(codistribute(Left(List("one", "two"))) == List(Left("one"), Left("two")))

}
