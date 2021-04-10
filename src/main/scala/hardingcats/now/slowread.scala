package hardingcats.now

import cats._
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object slowread extends App {

  val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))

  implicit val cs : ContextShift[IO] = IO.contextShift(ec)

  val timer : Timer[IO] = IO.timer(ec)

  val readSlow : IO[String] = IO(Thread.sleep(4000)) *> IO("content")

  def readSlow2() : IO[String] = IO {
    Thread.sleep(4000)
    "content"
  }

  val bg = IO(println(s"[${Thread.currentThread.getName}][${LocalDateTime.now}] tick"))

  val program : IO[Unit] = {
    for {

      res <- List(readSlow, readSlow, readSlow, readSlow).parSequence
    } yield ()
  }

}
