package fpdesign

/**
  *  - functor type class
  *  - laws
  *    - identity law     x.map(a => a) == x
  *    - composition law  x.map(f andThen g) == x.map(f).map(g)
  *  - covariant functor
  *  - contravariant functor
  */
object functors extends App {

  // * -> *  F[A]
  // Option[A], List[A]
  // Either[A, B]         * -> (* -> *)     F[A, B]    F[_, _]
  //  F[G[A], B]     :kind F[G[A], B] => (* -> *) -> * -> *

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

    implicit class FunctorOps[A, F[A]](val fa: F[A]) extends AnyVal {
      def fmap[B](f: A => B)(implicit ev: Functor[F]): F[B] = ev.map(fa)(f)
    }

    object instances {
      implicit val optionInstance: Functor[Option] = new Functor[Option] {
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      }

      type ThrowEither[A] = Either[Throwable, A] // * -> *

      implicit val eitherInstance: Functor[ThrowEither] = new Functor[ThrowEither] {
        def map[A, B](fa: ThrowEither[A])(f: A => B): ThrowEither[B] = fa.map(f)
      }

//      implicit val eitherInstance: Functor[Either[Throwable, *]] = ???

      implicit val listInstance: Functor[List] = new Functor[List] {
        def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
      }
    }

  }

  import Functor._
  import Functor.instances._

  println(Option(10).fmap(_ + 1))
  println(Functor[Option].map(Option(10))(_ + 1))

  def inc[F[_]: Functor](fa: F[Int]): F[Int] = fa.fmap(_ + 1)

  println(inc(Option(10)))
  println(inc(List(10, 11, 12, 13)))
  println(inc(Right(10): ThrowEither[Int]))
  println(inc(Left(new RuntimeException("ups")): ThrowEither[Int]))

  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    def apply[A: Show]: Show[A] = implicitly[Show[A]]

    implicit class ShowOps[A](val a: A) extends AnyVal {
      def show(implicit ev: Show[A]): String = ev.show(a)
    }

    def show[A](f : A => String) : Show[A] = (a: A) => f(a)

    object instances {

      implicit val functorInstance: Functor[Show] = new Functor[Show] {
        def map[A, B](fa: Show[A])(f: A => B): Show[B] = ???
      }

    }

  }

}
