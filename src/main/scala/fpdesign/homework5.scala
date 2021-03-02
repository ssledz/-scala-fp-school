package fpdesign

import fpdesign.functors.Functor

/**
  *  To read:
  *    - https://typelevel.org/cats/typeclasses/functor.html
  *    - https://typelevel.org/cats/typeclasses/contravariant.html
  *    - scala with cats book (read 1, 2, 3 chapters)
  *    - https://www.scala-exercises.org/cats/ (functor, monoid, semigroup)
  *    - implement
  *      - map3, map4
  */
object homework5 extends App {

  trait Applicative[F[_]] extends Functor[F] {

    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
    def pure[A](a: A): F[A]

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ???

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = ???

  }

}
