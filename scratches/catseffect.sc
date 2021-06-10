import cats._
import cats.effect.{Clock, ExitCase, IO, Sync}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

//-----IO monad------------
// both sync + async computation
val readInput: IO[String] = IO("a18")
//val readInput: IO[String] = IO(new BufferedReader(new InputStreamReader(System.in)).readLine())
def printIO(s: String): IO[Unit] = IO(println(s))

val program =
  for {
    _ <- printIO("What is your age?")
    age <- readInput.flatMap(s => IO(s.toInt))
    _ <- Monad[IO].ifM(IO.pure(age >= 18))(printIO("You are adult"), printIO("You are Kid"))
  } yield ()

program
  .handleErrorWith {
    case err: Throwable => printIO("Ups error: " + err)
  }
  .unsafeRunSync()

//------Clock-------------

val clock = Clock.create[IO]
println(clock.realTime(TimeUnit.MICROSECONDS).unsafeRunSync())
println(clock.realTime(TimeUnit.MICROSECONDS).unsafeRunSync())

//------------Sync---------

def myPrintLn[F[_]: Sync](message: String): F[Unit] = Sync[F].delay(println(message))

val ioHelloWorld = myPrintLn[IO]("HelloWorld")

case class LazyFuture[A](runUnsafeAsync: ExecutionContext => Future[A])

implicit val lazyFuture: Sync[LazyFuture] = new Sync[LazyFuture] {

  def suspend[A](thunk: => LazyFuture[A]) =
    LazyFuture { implicit ec =>
      Future.successful(()).flatMap(_ => thunk.runUnsafeAsync(ec))
    }

  def bracketCase[A, B](acquire: LazyFuture[A])(use: A => LazyFuture[B])(
      release: (A, ExitCase[Throwable]) => LazyFuture[Unit]) = ???

  def flatMap[A, B](fa: LazyFuture[A])(f: A => LazyFuture[B]) =
    LazyFuture(implicit ec => fa.runUnsafeAsync(ec).flatMap(a => f(a).runUnsafeAsync(ec)))

  override def tailRecM[A, B](a: A)(f: A => LazyFuture[Either[A, B]]) =
    flatMap(f(a)) {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => pure(value)
    }

  def raiseError[A](e: Throwable) = LazyFuture(_ => Future.failed(e))

  def handleErrorWith[A](fa: LazyFuture[A])(f: Throwable => LazyFuture[A]) = ???

  def pure[A](x: A): LazyFuture[A] = LazyFuture(_ => Future.successful(x))
}

val ec = scala.concurrent.ExecutionContext.global

val futureHelloWorld = myPrintLn[LazyFuture]("HelloWorld")

Await.result(futureHelloWorld.runUnsafeAsync(ec), Duration.Inf)



