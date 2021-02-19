package fpdesign

/**
  *  - functor type class
  *  - laws
  *    - identity law     x.map(a => a) == x
  *    - composition law  x.map(f andThen g) == x.map(f).map(g)
  *  - covariant functor
  *  - contravariant functor
  */
object functors extends App {}
