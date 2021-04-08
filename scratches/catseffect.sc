import cats._
import cats.effect.{Clock, ContextShift, IO, Timer}

import java.util.concurrent.{Executors, TimeUnit}

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


program.handleErrorWith {
  case err: Throwable => printIO("Ups error: " + err)
}.unsafeRunSync()

//------Clock-------------

val clock = Clock.create[IO]
println(clock.realTime(TimeUnit.MICROSECONDS).unsafeRunSync())
println(clock.realTime(TimeUnit.MICROSECONDS).unsafeRunSync())

