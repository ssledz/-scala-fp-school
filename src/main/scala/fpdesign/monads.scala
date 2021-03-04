package fpdesign

/**
 *  - monad type class
 *    - primitives: pure, flatMap
 *    - primitives: pure, map, join (def join[A](mma: F[F[A]]): F[A])
 *  - for comprehension for monads
 *  - example of monads
 *    - List
 *    - Option
 *    - Future
 *  - monads vs applicative
 *  - 'effects' in fp
 *  - monadic combinators
 *    - def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]]
 *  - laws
 *    - associative      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
 *    - left identity    flatMap(x)(pure) == x
 *    - right identity   flatMap(unit(y))(f) == f(y)
 *  - some good to know monads
 *    - Id monad
 *    - StateMonad
 *    - WriterMonad
 *    - ReaderMonad
 *  - composing monads
 *    - monads transformers
 *  - Kleisly arrows
 */
object monads {

}
