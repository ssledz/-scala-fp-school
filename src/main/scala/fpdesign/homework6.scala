package fpdesign

import fpdesign.applicative.Applicative
import fpdesign.functors.Functor

/**
  *  To read:
  *    - https://typelevel.org/cats/typeclasses/applicative.html
  *    - scala with cats book (read 6 chapters)
  *    - https://www.scala-exercises.org/cats/ (applicative)
  *    - http://learnyouahaskell.com/chapters
  *    - https://eed3si9n.com/herding-cats/
  *    - https://eed3si9n.com/learning-scalaz/
  *    - https://zio.dev/ (optional)
  *    - implement
  *      -
  */
object homework6 extends App {


  implicit val applicativeListInstance: Applicative[List] = ???

  implicit val applicativeEitherInstance: Applicative[Either[String, *]] = ???

  implicit val applicativeSetInstance: Applicative[Set] = ???

}
