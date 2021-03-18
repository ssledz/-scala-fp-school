import cats.effect.IO
import cats._
import cats.implicits._

import java.io.{BufferedReader, InputStreamReader}
// IO monad

val readInput: IO[String] = IO("a18")
//val readInput: IO[String] = IO(new BufferedReader(new InputStreamReader(System.in)).readLine())
def printIO(s : String) : IO[Unit] = IO(println(s))

val program =
  for {
    _ <- printIO("What is your age?")
    age <- readInput.flatMap(s => IO(s.toInt))
    _ <- Monad[IO].ifM(IO.pure(age >= 18))(printIO("You are adult"), printIO("You are Kid"))
  } yield ()


program.handleErrorWith {
  case err: Throwable => printIO("Ups error: " + err)
}.unsafeRunSync()