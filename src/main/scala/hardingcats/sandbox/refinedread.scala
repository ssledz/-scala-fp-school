package hardingcats.sandbox

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._

import java.time.LocalDateTime
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration._

object refinedread extends IOApp {

  def res: Resource[IO, ExecutorService] = Resource.make(IO(Executors.newCachedThreadPool()))(es => IO(println("Shutting down es")) *> IO(es.shutdown()))

  def slowlyReadFile(blocker: Blocker): IO[String] = blocker.blockOn(IO(Thread.sleep(10000)) *> IO.pure("content"))

  def run(args: List[String]): IO[ExitCode] = res.use { es =>
    val r = slowlyReadFile(Blocker.liftExecutorService(es))
    for {
      _ <- IO(println("Starting background task"))
      bj <- (IO(println(s"[${Thread.currentThread.getName}][${LocalDateTime.now}] tick")) *> Timer[IO].sleep(1.second)).foreverM.start
      _ <- Timer[IO].sleep(5.second) *> IO(println("Starting reading files"))
      as <- List(r, r, r).parSequence
      _ <- IO(println(s"Read: $as")) *> Timer[IO].sleep(5.second)
      _ <- bj.cancel
      _ <- IO(println("Cancelled"))
      _ <- Timer[IO].sleep(5.second)
    } yield ExitCode.Success
  }

}
