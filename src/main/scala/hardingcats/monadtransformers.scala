package hardingcats

import cats.data.OptionT

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  *  - monad composition
  *    - does monads compose?
  *  - OptionT
  *  - EitherT
  *  -
  */
object monadtransformers extends App {

  implicit class FutureOps[A](val fa: Future[A]) extends AnyVal {
    def get: A = Await.result(fa, Duration.Inf)
  }

  case class User(id: Long, login: String)
  case class Address(userId: Long, street: String)

  def findUserByLogin(login: String): Future[Option[User]] = login match {
    case "john.d" => Future.successful(Some(User(1, login)))
    case _        => Future.successful(None)
  }

  def findAddressByUserId(userId: Long): Future[Option[Address]] = userId match {
    case 1 => Future.successful(Some(Address(1, "Mickiewicza")))
    case _ => Future.successful(None)
  }

  def findStreetByLoginLegacy(login: String): Future[Option[String]] =
    findUserByLogin(login).flatMap {
      case Some(user) => findAddressByUserId(user.id).map(_.map(_.street))
      case None       => Future.successful(None)
    }

  def findStreetByLogin(login: String): Future[Option[String]] =
    (for {
      user <- OptionT(findUserByLogin(login))
      address <- OptionT(findAddressByUserId(user.id))
    } yield address.street).value

  println(findStreetByLogin("john.d").get)
  println(findStreetByLogin("alan.d").get)

  println(findStreetByLoginLegacy("john.d").get)
  println(findStreetByLoginLegacy("alan.d").get)

//  case class MonadT[A, F[_]: Monad, G[_]: Monad](value: F[G[A]]) {
//    def flatMap[B](f: A => F[G[B]]): F[G[B]] = ???
//    def map[B](f: A => G[B]): F[G[B]] = ???
//  }
//
//  def findStreetByLogin3(login: String): Future[Option[String]] =
//    for {
//      user <- MonadT[User, Future, Option](findUserByLogin(login))
//      address <- MonadT[Address, Future, Option](findAddressByUserId(user.id))
//    } yield address.street

}
