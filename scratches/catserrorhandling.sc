import cats._
import cats.implicits._
import cats.data._
import cats.effect.IO

import scala.util.{Failure, Success, Try}

case class Date(day: Int, month: Int, year: Int)

def parseDayEither(s: String): Either[Throwable, Int] = Try(s.split(' ').head.toInt).toEither
def parseMonthTry(s: String): Try[Int] = Try(s.split(' ')(1).toInt)
def parseYearOption(s: String): Option[Int] = Try(s.split(' ')(2).toInt).toOption

def parseDateTry(s: String): Try[Date] =
    for {
      day <- parseDayEither(s).toTry
      month <- parseMonthTry(s)
      year <- parseYearOption(s) match {
        case Some(value) => Success(value)
        case None        => Failure(new RuntimeException(s"Error during parsing date: $s"))
      }
    } yield Date(day, month, year)

println(parseDateTry("12 01 2021"))

def parseDay[F[_]: ApplicativeThrow](s: String): F[Int] = ApplicativeThrow[F].catchNonFatal(s.split(' ').head.toInt)
def parseMonth[F[_]: ApplicativeThrow](s: String): F[Int] = ApplicativeThrow[F].catchNonFatal(s.split(' ')(1).toInt)
def parseYear[F[_]: ApplicativeThrow](s: String): F[Int] = ApplicativeThrow[F].catchNonFatal(s.split(' ')(2).toInt)

def parseDate[F[_]: MonadThrow](s: String): F[Date] =
    for {
      d <- parseDay[F](s)
      m <- parseMonth[F](s)
      y <- parseYear[F](s)
      _ <- if (m > 12) new IllegalArgumentException(s"month is greater than 12").raiseError[F, Throwable]
      else ().pure[F]
    } yield Date(d, m, y)

println(parseDate[({ type F[A] = Either[Throwable, A] })#F]("12 01 2021"))
println(parseDate[Try]("12 01 2021"))
println(parseDate[IO]("12 01 2021").unsafeRunSync())
println(parseDate[IO]("12 13 2021").attempt.unsafeRunSync())
