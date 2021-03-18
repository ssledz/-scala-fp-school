import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits._
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

// Eval
  val foo = Eval.always {
    println("Computing")
    1
  }

println("Foo computing")
foo.value
foo.value
val bar = foo.memoize
println("Bar computing")
bar.value
bar.value

// Writer

type Logged[A] = Writer[List[String], A]

def multiply(a: Int, b: Int): Logged[Int] =
    Writer(List(s"$a * $b"), a * b)

def doSomething(a: Int, b: Int): Writer[List[String], Int] =
    for {
      c <- Writer.value[List[String], Int](a + b)
      _ <- Writer.tell(List(s"$a + $b"))
      _ <- List("some comment").tell
      d <- multiply(c, 4)
    } yield d

println(doSomething(1, 2).run)

// State

var seq = 0
def stateFullNextInt: Int = {
    seq = seq + 1
    seq
  }

println(stateFullNextInt)
println(stateFullNextInt)
println(stateFullNextInt)

def betterNextInt(seq: Int): (Int, Int) = (seq + 1, seq)

val (seq1, v1) = betterNextInt(1)
val (seq2, v2) = betterNextInt(seq1)
val (seq3, v3) = betterNextInt(seq2)

println(v1)
println(v2)
println(v3)

val nextInt: State[Int, Int] =
    for {
      value <- State.get[Int]
      _ <- State.set(value + 1)
    } yield value

type IntState[A] = State[Int, A]

val xs: List[IntState[Int]] = List.fill(3)(nextInt)

println(xs.sequence.runA(0).value)

// NonEmptyList
  val ys = NonEmptyList.of(1, 2, 3, 4)
val zs: List[Int] = ys.tail
println(ys)
println(ys.head)
println(zs)

// Chain
  val ws = Chain.one(1) ++ Chain.one(2)
println(ws)

// Validated

val rs: Validated[List[String], String] = Validated.valid("valid name")
val ss: Validated[List[String], String] = Validated.valid("valid email")
val ss2 = Validated.invalid(List("invalid email"))

case class User(name: String, email: String)

println((rs, ss).mapN(User.apply))
println((rs, ss2).mapN(User.apply))

// Reader
  type Time = Long

def zoned(t: Time): ZonedDateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(t), ZoneId.of("CET"))

def day(t: Time): Int = zoned(t).getDayOfYear
def hour(t: Time): Int = zoned(t).getHour
def minute(t: Time): Int = zoned(t).getMinute

def format(t: Time): String = {
    val d = day(t)
    val h = hour(t)
    val m = minute(t)
    s"day $d, $h:$m"
  }

println(format(System.currentTimeMillis()))

val zonedR: Reader[Time, ZonedDateTime] = Reader(zoned)
val dayR: Reader[Time, Int] = zonedR.map(_.getDayOfYear)
val hourR: Reader[Time, Int] = zonedR.map(_.getHour)
val minuteR: Reader[Time, Int] = zonedR.map(_.getMinute)

val betterFormat: Reader[Time, String] =
    for {
      d <- dayR
      h <- hourR
      m <- minuteR
    } yield s"day $d, $h:$m"

println(betterFormat.run(System.currentTimeMillis()))

// Id monad

val ps: Id[Int] = 1
val qs: Id[Int] = 2

def add[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] =
    for {
      a <- fa
      b <- fb
    } yield a + b

println(add(ps, qs))

// Kleisli
// Reader[Time, String] === Kleisli[Id, Time, String]

val response : Kleisli[Future, String, Int] = Kleisli[Future, String, Int] {
  req => Future.successful(req.toInt)
}

println(Await.result(response.run("123"), Duration.Inf))