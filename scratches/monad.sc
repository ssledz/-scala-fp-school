trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f.andThen(pure))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

}

trait Monad2[F[_]] {

  def pure[A](a: A): F[A]
  def join[A](ffa: F[F[A]]): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

}

object Monad {
  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  implicit class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Monad[F]): F[B] = F.map(fa)(f)
    def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)
  }

  implicit class MonadOps2[A](val a: A) extends AnyVal {
    def pure[F[_]: Monad](implicit F: Monad[F]): F[A] = F.pure(a)
  }

  implicit class MonadOps3[F[_], A](val ffa: F[F[A]]) extends AnyVal {
    def join(implicit F: Monad[F]): F[A] = F.join(ffa)
  }

  object instances {
    implicit val optionInstance: Monad[Option] = new Monad[Option] {
      def pure[A](a: A): Option[A] = Option(a)
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
  }

}

import Monad._
import Monad.instances._

import java.time.LocalDateTime

def sum[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] =
    for {
      a <- fa
      b <- fb
    } yield a + b

sum(1.pure, 2.pure)
1.pure.pure
1.pure.pure.join

def increment(a: Int): (List[String], Int) = (List(s"incrementing $a by 1"), a + 1)
def multiply(a: Int, b: Int): (List[String], Int) = (List(s"multiply $a by $b"), a * b)

def someOperations(a: Int): (List[String], Int) = {
    val (log1, a1) = increment(a)
    val (log2, a2) = multiply(a1, 3)
    (log1 ::: log2, a2)
  }

def someIntensiveOperations(a: Int): (List[String], Int) = {
    val (log1, a1) = someOperations(1)
    val (log2, a2) = increment(a1)
    (log1 ::: log2, a2)
  }

println(someOperations(2))
println(someIntensiveOperations(2))

trait Monoid[A] {
  def combine(a: A, b: A): A
  def mempty: A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
}

case class WriterMonad[L, A](run: (L, A)) {

  def map[B](f: A => B)(implicit m: Monoid[L]): WriterMonad[L, B] = flatMap(f.andThen(WriterMonad.pure[L, B]))

  def flatMap[B](f: A => WriterMonad[L, B])(implicit m: Monoid[L]): WriterMonad[L, B] =
    WriterMonad[L, B] {
      val (l1, a1) = run
      val (l2, a2) = f(a1).run
      (m.combine(l1, l2), a2)
    }

}

object WriterMonad {
  def pure[L: Monoid, A](a: A): WriterMonad[L, A] = WriterMonad(Monoid[L].mempty, a)

  def tell[L](l: L): WriterMonad[L, Unit] = WriterMonad(l, ())
}

implicit def listMonoidInstance[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def combine(a: List[A], b: List[A]): List[A] = a ++ b
    def mempty: List[A] = List.empty
  }

type Logged[A] = WriterMonad[List[String], A]

def increment2(a: Int): Logged[Int] = WriterMonad(List(s"incrementing $a by 1"), a + 1)
def multiply2(a: Int, b: Int): Logged[Int] = WriterMonad(List(s"multiply $a by $b"), a * b)

def someOperations2(a: Int): Logged[Int] =
    for {
      a1 <- increment2(a)
      _ <- WriterMonad.tell(List("time: " + LocalDateTime.now))
      a2 <- multiply2(a1, 3)
    } yield a2

println(someOperations2(1))

def length(s: String): Int = s.length
def prefixWith(s: String, prefix: String): String = prefix + s
def postfixWith(s: String, postfix: String): String = s + postfix

println(length(prefixWith("foo", "barPrefix-")))
println(length(postfixWith(prefixWith("foo", "barPrefix-"), "-posfix")))

case class ReaderMonad[E, +A](run: E => A) {
  def flatMap[B](f: A => ReaderMonad[E, B]): ReaderMonad[E, B] = ReaderMonad { e =>
    val a = run(e)
    f(a).run(e)
  }

  def map[B](f: A => B): ReaderMonad[E, B] = flatMap(a => ReaderMonad.pure(f(a)))
}

object ReaderMonad {

  def pure[E, A](a: A): ReaderMonad[E, A] = ReaderMonad(_ => a)

  def ask[E]: ReaderMonad[E, E] = ReaderMonad(identity)

  def asks[E, A](f: E => A): ReaderMonad[E, A] = ReaderMonad(e => f(e))

  def local[E, A](r: ReaderMonad[E, A])(f: E => E): ReaderMonad[E, A] = ReaderMonad(e => r.run(f(e)))
}

val length2: ReaderMonad[String, Int] = ReaderMonad.ask[String].map(_.length)

val xx1: String => String = prefixWith(_, "prefix-")
val xx2: String => String = postfixWith(_, "prefix-")
val xx3: String => String = xx1.andThen(xx2)

println(ReaderMonad.local(length2)(xx3).run("foo"))

type Env = Map[String, Int]

case class Kleisli[A, B, F[_]](f: A => F[B])

case class StateMonad[S, A](run: S => (S, A))
object Id {
  type Id[A] = A
}
