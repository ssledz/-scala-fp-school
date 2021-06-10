package hardingcats.sandbox

import cats.effect.{ExitCase, IO, Sync}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import cats.implicits._

object lazyfuture extends App {

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

  def myPrintLn[F[_]: Sync](message: String): F[Unit] = Sync[F].delay(println(message))

  val futureHelloWorld = for {
    _ <- myPrintLn[LazyFuture]("Hello")
    _ <- myPrintLn[LazyFuture]("World!")
  } yield ()

//  Await.result(futureHelloWorld.runUnsafeAsync(ec), Duration.Inf)

  val ioHelloWorld = myPrintLn[IO]("Hello World!")

  ioHelloWorld.unsafeRunSync()

  Thread.sleep(1000)

}
