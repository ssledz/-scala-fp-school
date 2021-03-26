package hardingcats

import cats._
import cats.data.Reader
import cats.effect.IO
import cats.syntax.all._

import scala.util.Try

/**
  *  - MonadError type class
  *  - raising and handling errors
  */
object errorhandling extends App {

  val str = "20-11-2021"

  case class Date(day: Int, month: Int, year: Int)

  def parseDay2[F[_]: Applicative](s: String): F[Int] = s.split('-').headOption.map(_.toInt).get.pure[F]

  def parseDay[F[_]: ApplicativeThrow](s: String): F[Int] =
    s.split('-').headOption.flatMap(x => Try(x.toInt).toOption) match {
      case Some(value) => value.pure[F]
      case None        => ApplicativeThrow[F].raiseError(new IllegalArgumentException(s"Error during parsing s: '$s'"))
    }

  def parseMonth[F[_]: ApplicativeThrow](s: String): F[Int] =
    ApplicativeThrow[F].catchNonFatal(s.split('-').tail.head.toInt)
  def parseYear[F[_]: ApplicativeThrow](s: String): F[Int] =
    ApplicativeThrow[F].catchNonFatal(s.split('-').tail.tail.head.toInt)

  println(parseDay[Try](str))
  println(parseDay[Either[Throwable, *]](""))
  println(parseMonth[Try]("20-11-2021"))
  println(parseYear[Try]("20-11-2021"))

  // Try, Either, IO => F[_]
  def parse[F[_]: MonadThrow](s: String): F[Date] =
    for {
      d <- parseDay(s)
      m <- parseMonth(s)
      y <- parseYear(s)
    } yield Date(d, m, y)

  println(parse[Either[Throwable, *]](str))

  println(parse[Either[Throwable, *]]("str"))

  val foo: IO[Date] = parse[IO]("str")
  val bar: IO[Either[Throwable, Date]] = parse[IO]("str").attempt

  println(bar.unsafeRunSync())

  def parseDayR[F[_]: ApplicativeThrow]: Reader[String, F[Int]] = Reader(parseDay[F])
  def parseMonthR[F[_]: ApplicativeThrow]: Reader[String, F[Int]] = Reader(parseMonth[F])
  def parseYearR[F[_]: ApplicativeThrow]: Reader[String, F[Int]] = Reader(parseYear[F])

  def parseR[F[_]: MonadThrow]: Reader[String, F[Date]] =
    for {
      d <- parseDayR
      m <- parseMonthR
      y <- parseYearR
    } yield (d, m, y).mapN(Date)

  println(parseR[Try].run(str))

}
