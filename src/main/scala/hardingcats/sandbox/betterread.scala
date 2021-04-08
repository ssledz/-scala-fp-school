package hardingcats.sandbox

import cats.effect.{Blocker, ContextShift, IO, Timer}
import cats.implicits._

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object betterread extends App {

  val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  val es = Executors.newCachedThreadPool
  val blocker = Blocker.liftExecutorService(es)
  implicit val timer: Timer[IO] = IO.timer(ec)

  def slowlyReadFile(blocker: Blocker)(implicit cs: ContextShift[IO]): IO[String] =
    blocker.blockOn(IO(Thread.sleep(10000)) *> IO.pure("content"))

  val program = {
    val r = slowlyReadFile(blocker)
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
  es.shutdown()

}
