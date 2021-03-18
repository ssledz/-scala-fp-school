
import scala.util.{Failure, Success}

// bottom type
sealed trait List[+A]

class Cons[A](a: A, tai: List[A]) extends List[A]
object Nil extends List[Nothing] // bottom type

val xs: List[Int] = Nil

def loopForever: Nothing = {
    while (true) {}
    throw new IllegalStateException("should not occur")
  }

// variances

trait MyFunction[-A, +B] {
  def apply(a: A): B
}

trait MyString
trait Text extends MyString

trait Number
trait BigNumber extends Number

def f: MyFunction[MyString, Number] = ???
def g: MyFunction[MyString, BigNumber] = ???
def i: MyFunction[Text, BigNumber] = ???

def h1: MyFunction[MyString, Number] = f
def h2: MyFunction[MyString, Number] = g

// def h3 : MyFunction[MyString, Number] = i // can't be

def h4: MyFunction[Text, Number] = i
def h5: MyFunction[Text, Number] = g

// generic classes
class Foo[A] // Foo is type constructor
def foo: Foo[Int] = ??? // Foo[Int] is a concrete type

class Foo2[A, B, C] // Foo2 is a type constructor

object Env {
  type Foo2Int[B, C] = Foo2[Int, B, C]
  type Foo2IntString[C] = Foo2[Int, String, C]
  type Foo2IntStringInt = Foo2[Int, String, Int]
}

// higher-kinded type

// functors

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// contravariant functor

trait ContravariantFunctor[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

trait Show[A] {
  def show(a: A): String
}

object ContravariantFunctor {
  implicit class ContravariantFunctorOps[F[_], A](val fa: F[A]) extends AnyVal {
    def contramap[B](f: B => A)(implicit ev: ContravariantFunctor[F]): F[B] =
      ev.contramap(fa)(f)
  }
}

object Show {
  def apply[A: Show]: Show[A] = implicitly[Show[A]]

  def show[A](f: A => String): Show[A] = (a: A) => f(a)

  implicit class ShowOps[A](val a: A) extends AnyVal {
    def show(implicit ev: Show[A]): String = ev.show(a)

  }

  implicit val contraShowInstance: ContravariantFunctor[Show] = new ContravariantFunctor[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show[B](b => fa.show(f(b)))
  }

}

case class Money(amount: Int)

object Money {
  implicit val showInstance: Show[Money] = (a: Money) => s"amount: $a"
}

import Show._
import basics.implicits.SemiGroup

import scala.util.Try

case class Bag(money: Money)

object Bag {
  import ContravariantFunctor._
  implicit val showInstance: Show[Bag] = Show[Money].contramap(_.money)
}

println(Bag(Money(123)).show)

// applicative functor

trait Applicative[F[_]] extends Functor[F] {

  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val cf: A => B => C = f.curried
    val fabc: F[A => B => C] = pure(cf)
    val fbc: F[B => C] = ap(fabc)(fa)
    val fc: F[C] = ap(fbc)(fb)
    fc
  }

  def sequence[A](xs: scala.List[F[A]]): F[scala.List[A]] =
    map(xs.foldLeft(pure(List.empty[A])) { (acc, fa) =>
      map2(fa, acc)(_ :: _)
    })(_.reverse)

  def traverse[A, B](xs: scala.List[A])(f: A => F[B]): F[scala.List[B]] = sequence(xs.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[scala.List[A]] = sequence(scala.List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)(_ -> _)

  def filterM[A](xs: scala.List[A])(f: A => F[Boolean]): F[scala.List[A]] =
    map(xs.foldLeft(pure(scala.List.empty[A])) { (acc, a) =>
      map2(acc, f(a)) {
        case (acc2, true)  => a :: acc2
        case (acc2, false) => acc2
      }
    })(_.reverse)

}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  implicit class ApplicativeOps[F[_], A](val fa: F[A]) extends AnyVal {
    def fmap[B](f: A => B)(implicit ev: Applicative[F]): F[B] = ev.map(fa)(f)
    def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit ev: Applicative[F]): F[C] = ev.map2(fa, fb)(f)

    def ap[B, C](fb: F[B])(implicit ev: Applicative[F], ev2: A =:= (B => C)): F[C] =
      ev.ap(fa.asInstanceOf[F[B => C]])(fb)

  }

  implicit val optionInstance: Applicative[Option] = new Applicative[Option] {
    def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)

    def pure[A](a: A): Option[A] = Option(a)
  }
}

import Applicative._

val strToInt: String => Int = _.toInt

Option(strToInt).ap(Some("1234"))

trait SemiGroup[A] {
  def combine(a: A, b: A): A
}

object Semigroup {
  def apply[A: SemiGroup]: SemiGroup[A] = implicitly[SemiGroup[A]]
}

sealed trait Validated[+E, +A] {

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a)       => Valid(f(a))
    case i @ InValid(_) => i
  }

  def combine[EE >: E, AA >: A](other: Validated[EE, AA])(implicit EE: SemiGroup[EE],
                                                          AA: SemiGroup[AA]): Validated[EE, AA] =
    (this, other) match {
      case (Valid(a), Valid(b))     => Valid(AA.combine(a, b))
      case (InValid(a), InValid(b)) => InValid(EE.combine(a, b))
      case (Valid(_), _)            => other
      case _                        => this
    }

  def ap[EE >: E, B](ff: Validated[EE, A => B])(implicit EE: SemiGroup[EE]): Validated[EE, B] = (this, ff) match {
    case (Valid(a), Valid(f))         => Valid(f(a))
    case (InValid(a), InValid(b))     => InValid(EE.combine(a, b))
    case (Valid(_), err @ InValid(_)) => err
    case (err @ InValid(_), _)        => err
  }

  def map2[EE >: E, B, C](fb: Validated[EE, B])(f: (A, B) => C)(implicit EE: SemiGroup[EE]): Validated[EE, C] = {
    val fabc: Validated[EE, A => B => C] = Validated.pure(f.curried)
    val fbc: Validated[EE, B => C] = ap(fabc)
    fb.ap(fbc)
  }

  def toEither: Either[E, A] = this match {
    case Valid(a)     => Right(a)
    case InValid(err) => Left(err)
  }

}

object Validated {

  def pure[A](a: A): Validated[Nothing, A] = Valid(a)

  def invalid[A, E](e: E): Validated[E, A] = InValid(e)

  implicit def applicativeFunctor[E: SemiGroup]: Applicative[({ type f[A] = Validated[E, A] })#f] = ???
}

case class Valid[A](a: A) extends Validated[Nothing, A]

case class InValid[E](e: E) extends Validated[E, Nothing]

object SomeDomain {

  type ValidatedResult[A] = Validated[scala.List[ValidationError], A]

  sealed trait ValidationError

  case object AgeIsNotANumber extends ValidationError
  case object NameIsTooLong extends ValidationError
  case object AgeIsTooSmall extends ValidationError

  case class Adult(age: Int, name: String)

  def validateAge(age: String): ValidatedResult[Int] =
    Try(age.toInt) match {
      case Failure(_)                    => Validated.invalid(scala.List(AgeIsNotANumber))
      case Success(value) if value >= 18 => Validated.pure(value)
      case Success(_)                    => Validated.invalid(scala.List(AgeIsTooSmall))
    }

  def validateName(name: String): ValidatedResult[String] =
    if (name.length >= 5) Validated.invalid(List(NameIsTooLong)) else Validated.pure(name)

  implicit def listSemigroup[A]: SemiGroup[scala.List[A]] = new SemiGroup[scala.List[A]] {
    def combine(a: scala.List[A], b: scala.List[A]): scala.List[A] = a ::: b
  }

}

import SomeDomain.{Adult, validateAge, validateName}

val ageValidation = validateAge("11")
val nameValidation = validateName("a name")

val adult1 = ageValidation.map2(nameValidation)(Adult.apply)
validateAge("11").map2(validateName("a"))(Adult.apply)

val adult2 =
    for {
      age <- ageValidation.toEither
      name <- nameValidation.toEither
    } yield Adult(age, name)

println(adult1)
println(adult2)

Applicative[Option].sequence(List(Option(1), Option(2)))
Applicative[Option].sequence(List(Option(1), None))

def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

Applicative[Option].traverse(List("1", "2", "3", "4", "5"))(parseInt)
Applicative[Option].traverse(List("1", "2", "a"))(parseInt)

Applicative[Option].replicateM(5, Some(1))
Applicative[Option].replicateM(5, None)

def even(s : String) : Option[Boolean] = parseInt(s).map(_ % 2 == 0)

Applicative[Option].filterM(List("1" ,"2", "3", "4"))(even)
Applicative[Option].filterM(List("1" ,"2a", "3", "4"))(even)