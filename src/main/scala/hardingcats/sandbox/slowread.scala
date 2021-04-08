package hardingcats.sandbox

import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object slowread extends App {

  val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  implicit val timer: Timer[IO] = IO.timer(ec)

  def slowlyReadFile(ec: ExecutionContext)(implicit cs: ContextShift[IO]): IO[String] =
    cs.evalOn(ec)(IO(Thread.sleep(10000)) *> IO.pure("content"))

  val program = {
    val r = slowlyReadFile(ec)
    for {
      _ <- IO(println("Starting background task"))
      bj <- (IO(println(s"[${Thread.currentThread.getName}][${LocalDateTime.now}] tick")) *> Timer[IO].sleep(1.second)).foreverM.start
      _ <- Timer[IO].sleep(5.second) *> IO(println("Starting reading files"))
      as <- List(r, r, r).parSequence
      _ <- IO(println(s"Read: $as")) *> Timer[IO].sleep(5.second)
      _ <- bj.cancel
      _ <- IO(println("Cancelled"))
    } yield ()
  }

  program.unsafeRunSync()
  println("Waiting....")
  Thread.sleep(5000)
  ec.shutdown()

}
