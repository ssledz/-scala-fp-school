package fpdesign

import fpdesign.functors.Functor

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  *  - applicative type class
  *  - example of applicative instance
  *    - option
  *  - validated data type
  *  - validated applicative instance
  *  - validated vs either
  *   - applicative combinators
  *     - def sequence[A](xs: List[F[A]]): F[List[A]]
  *     - def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]]
  *     - def replicateM[A](n: Int, fa: F[A]): F[List[A]]
  *     - def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)]
 *      - def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]]
  *  - laws
  *    - functor laws (identity, composition)
  *    - left identity   map2(unit(()), fa)((_,a) => a) == fa
  *    - right identity  map2(fa, unit(()))((a,_) => a) == fa
  *    - associativity   product(product(fa,fb),fc) == map(product(fa, product(fb,fc)))(assoc)
  */
object applicative {

  // * -> *
  trait Applicative[F[_]] extends Functor[F] {
    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      val fab: F[A => B] = pure(f)
      ap(fab)(fa)
    }

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      val fabc: F[A => B => C] = pure(f.curried)
      val bc: F[B => C] = ap(fabc)(fa)
      ap(bc)(fb)
    }

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ???
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = ???

  }

  object Applicative {

    def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

    implicit val futureInstance: Applicative[Future] = ???
    implicit val optionInstance: Applicative[Option] = ???
  }

  import Applicative._

  case class Computation(a: Int, b: String, c: Float)

  import concurrent.ExecutionContext.Implicits._

  val computationF: Future[Computation] =
    Applicative[Future].map3(Future(1), Future("1"), Future(1.1f))(Computation.apply)

  val computation: Computation = Await.result(computationF, Duration.Inf)

  val computationF2: Option[Computation] = Applicative[Option].map3(Some(1), Some("1"), None)(Computation.apply)

}
