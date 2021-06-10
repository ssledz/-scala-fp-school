package hardingcats.sandbox

import cats.effect.{Async, ExitCode, IO, IOApp, Sync}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object async extends IOApp {

  def restCall: IO[String] = IO(println("[dbg] calling rest endpoint")) *> IO.sleep(2000.millis) *> IO("Rest response")

  def run(args: List[String]): IO[ExitCode] =
    for {
      restCallMem <- Async.memoize(restCall)
//     _ <- program(restCall)
//      _ <- program(restCallMem)
      cassandraApi <- LiveCassandraApi.make[IO]
      res <- cassandraApi.query("select * from user")
      _ <- IO(println("res: " + res))
    } yield ExitCode.Success

  def program(restCall: IO[String]): IO[Unit] =
    for {
      r1 <- restCall
      r2 <- restCall
      _ <- IO(println("Response: " + List(r1, r2).mkString(",")))
    } yield ()

  case class ResultSet(rows: List[Map[String, String]])

  def queryCassandraSync(query: String): ResultSet = {
    Thread.sleep(4000)
    ResultSet(List(Map("echo" -> query)))
  }

  def queryCassandraAsync(query: String)(cb: Either[Throwable, ResultSet] => Unit): Unit = {
    import concurrent.ExecutionContext.Implicits._
    Future(queryCassandraSync(query)).onComplete {
      case Failure(exception) => cb(Left(exception))
      case Success(value)     => cb(Right(value))
    }
  }

  trait CassandraApi[F[_]] {
    def query(q: String): F[ResultSet]
  }

  object LiveCassandraApi {
    def make[F[_]: Async]: F[CassandraApi[F]] = Sync[F].delay(new LiveCassandraApi[F])
  }

  private class LiveCassandraApi[F[_]: Async] extends CassandraApi[F] {
    def query(q: String): F[ResultSet] = Async[F].async(cb => queryCassandraAsync(q)(cb))
  }

}

object legacyapp extends App {

  val start = System.currentTimeMillis()
  val res = async.queryCassandraSync("select * from user")
//  val res = async.queryCassandraAsync("select * from user")(println)
  println(res)
  println("Start doing some cpu intensive tasks...")
  Thread.sleep(5000)
  val d = (System.currentTimeMillis() - start) / 1000.0
  println(s"end in $d s")
}
