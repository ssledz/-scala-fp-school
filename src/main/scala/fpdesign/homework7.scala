package fpdesign

import fpdesign.functors.Functor

/**
  *  To read:
  *    - Read  about Donald Knuth (https://en.wikipedia.org/wiki/Donald_Knuth)
  *       - https://en.wikipedia.org/wiki/MMIX
 *     - https://typelevel.org/cats/
 *       - https://typelevel.org/cats/typeclasses.html
 *       - https://typelevel.org/cats/datatypes.html
 *       - https://typelevel.org/cats/resources_for_learners.html
 *       - https://typelevel.org/cats/guidelines.html
 *     - https://typelevel.org/cats-effect/
 *       - https://typelevel.org/cats-effect/tutorial/tutorial.html
 *       - https://typelevel.org/cats-effect/concurrency/basics.html
 *       - https://typelevel.org/cats-effect/concurrency/
 *
  *  Implement
  *      - def replicateM[A](n: Int, fa: F[A]): F[List[A]]
  *       - def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]]
  */
object homework7 extends App {

  trait Applicative[F[_]] extends Functor[F] {

    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
    def pure[A](a: A): F[A]

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???
  }

}
