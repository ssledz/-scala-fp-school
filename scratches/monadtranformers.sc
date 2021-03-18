import cats._
import cats.data.EitherT
import cats.implicits._

import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration

case class User(id: Int, name: String)

def findAllUsers: Future[Either[Throwable, List[User]]] =
    Future.successful(Right(List(User(1, "Ron"), User(2, "Hermiona"))))
def findAgeForUser(id: Int): Future[Either[Throwable, Int]] = Future.successful(Right(id + 1))

case class UserAge(name: String, age: Int)

def getUserAge(name: String): Future[Either[Throwable, UserAge]] =
    for {
      usersOrErr <- findAllUsers
      ageOrError <- usersOrErr match {
        case Left(err) => Future.successful(err.asLeft[Int])
        case Right(users) =>
          users.find(_.name == name) match {
            case Some(user) => findAgeForUser(user.id)
            case None       => Future.successful(new RuntimeException(s"No such user like $name").asLeft[Int])
          }
      }
    } yield ageOrError.map(age => UserAge(name, age))

def getUserAge2(name: String): Future[Either[Throwable, UserAge]] =
    (for {
      users <- EitherT(findAllUsers)
      age <- for {
        user <- EitherT(
          Future.successful(users.find(_.name == name).toRight(new RuntimeException(s"No such user like $name"))))
        age <- EitherT(findAgeForUser(user.id))
      } yield age

    } yield UserAge(name, age)).value

println(Await.result(getUserAge("Ron"), Duration.Inf))
println(Await.result(getUserAge2("Ron"), Duration.Inf))