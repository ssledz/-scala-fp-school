package hardingcats.now

import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._
import hardingcats.now.slowread.{bg, ioPrintln, readSlow}

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object slowread extends App {

  val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))
  val ex = Executors.newCachedThreadPool()
  val blocker = Blocker.liftExecutorService(ex)

  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  implicit val timer: Timer[IO] = IO.timer(ec)

  def ioPrintln(m: String): IO[Unit] = IO(println(s"[${Thread.currentThread().getName}] - $m"))

  def readSlow(n: Int): IO[String] =
    ioPrintln(s"Start reading$n...") *> IO(Thread.sleep(4000)) *>
      ioPrintln(s"Finished reading$n...") *> IO("content")

  def readSlow2(): IO[String] = IO {
    Thread.sleep(4000)
    "content"
  }

  val bg = IO(println(s"[${Thread.currentThread.getName}][${LocalDateTime.now}] tick")) *> Timer[IO].sleep(1000.millis)

  val program: IO[Unit] = {
    val start = System.currentTimeMillis()
    for {
      _ <- bg.foreverM.start
//      res <- List(blocker.blockOn(readSlow(1)),
//                  blocker.blockOn(readSlow(2)),
//                  blocker.blockOn(readSlow(3)),
//                  blocker.blockOn(readSlow(4))).parSequence
      res <- List(readSlow(1), readSlow(2), readSlow(3), readSlow(4)).map(a => blocker.blockOn(a)).parSequence
      duration = (System.currentTimeMillis() - start) / 1000.0
      _ <- ioPrintln("res: " + res.mkString(","))
      _ <- ioPrintln(s"duration: $duration s")
    } yield ()
  }

  program.unsafeRunSync()
  Thread.sleep(5000)
  ec.shutdown()
  ex.shutdown()
}

object refinedslowread extends IOApp {

  val blockerRes: Resource[IO, Blocker] = (Resource
    .make(
      IO {
        val ex = Executors.newCachedThreadPool()
        ex -> Blocker.liftExecutorService(ex)
      }
    ) { case (ex, _) => IO(ex.shutdown()) })
    .map(_._2)

  val bg = IO(println(s"[${Thread.currentThread.getName}][${LocalDateTime.now}] tick")) *> Timer[IO].sleep(1000.millis)

  def run(args: List[String]): IO[ExitCode] =
    blockerRes.use { blocker =>
      val start = System.currentTimeMillis()
      for {
      _ <- IO.unit
        _ <- bg.foreverM.start
        res <- List(readSlow(1), readSlow(2), readSlow(3), readSlow(4)).map(a => blocker.blockOn(a)).parSequence
        duration = (System.currentTimeMillis() - start) / 1000.0
        _ <- ioPrintln("res: " + res.mkString(","))
        _ <- ioPrintln(s"duration: $duration s")
      } yield ExitCode.Success
    }
}
