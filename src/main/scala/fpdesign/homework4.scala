package fpdesign

import basics.homework2.Monoid

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
  *    - implement foldMap, intMonoidInstance, vectorFunctorInstance to assert a given expression
  *    - implement distribute and codistribute
  */
object homework4 extends App {

  import functors.Functor

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

  def foldMap[F[_]: Functor, A, B: Monoid](fa: F[A])(f: A => B): B = ???

  case class Money(value: Int)
  implicit val intMonoidInstance: Monoid[Int] = ???
  implicit val vectorFunctorInstance: Functor[Vector] = ???

  assert(foldMap(Vector(Money(1), Money(2), Money(3)))(_.value) == 6)

  def distribute[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = ???

  def codistribute[F[_]: Functor, A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = ???

}
