package hardingcats

import cats.effect.IO

import java.io.{BufferedReader, InputStreamReader}

/**
  *  - data types
  *    - IO
  *    - SyncIO
  *    - Fiber
  *    - Resource
  *    - Clock
  *    - ContextShift
  *    - Timer
  *    - IOApp
  *    - Blocker
  *  - tagless final encoding
  *  - type classes
  *    - Sync
  *    - Async
  *    - Concurrent
  *    - Effect
  *    - ConcurrentEffect
  *    - Bracket
  *    - LiftIO
  *  - concurrency
  *    - Ref
  *    - MVar
  *    - Semaphore
  *    - Deferred
  */
object catseffectintro extends App {

  val hello: IO[Unit] = IO(println("Hello IO monad"))

  hello.unsafeRunSync()

  hello.unsafeRunSync()
  hello.unsafeRunSync()

//  val readInput: IO[String] = IO(new BufferedReader(new InputStreamReader(System.in)).readLine())
  val readInput: IO[String] = IO.pure("18")
  def printConsole(msg: String): IO[Unit] = IO(println(msg))
//  val readInput2 : IO[String] = IO(Source.stdin.getLines().mkString("\n"))

  val program =
    for {
      _ <- printConsole("What is your age?")
      sage <- readInput
      maybeAge <- IO(sage.toInt).attempt
      _ <- maybeAge match {
        case Right(age) if age < 18 => printConsole("You are kid")
        case Right(age)             => printConsole(s"You are adult because $age > 18")
        case Left(err)              => printConsole(s"age '$sage' is not a number: " + err.getMessage)
      }
//       _ <- if(age < 18) printConsole("You are kid") else printConsole(s"You are adult because $age > 18")
      _ <- printConsole("Bye")
    } yield ()

  program
    .handleErrorWith {
      case err => printConsole(s"Error: " + err.getMessage)
    }
    .unsafeRunSync()

  println("End")


}
