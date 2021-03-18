{
    import cats._ // imports all of Cats’ type classes in one go
    import cats.implicits._ // imports all of the standard type class instances and all of the syntax in one go.

    case class Foo(a: Int, s: String)

    object Foo {
      implicit val showInstance: Show[Foo] = Show.fromToString
    }
    println(Foo(11, "bar").show)

  }

{
    import cats._ // imports all of Cats’ type classes in one go
    import cats.syntax.all._ // imports all of the syntax in one go

    case class Bar(a: Int, m: Option[String])

    object Bar {
      implicit val semigroupInstance: Semigroup[Bar] = new Semigroup[Bar] {
        def combine(x: Bar, y: Bar): Bar = Bar(x.a + y.a, x.m.flatMap(xx => Some(xx + y.m.getOrElse(""))).orElse(y.m))
      }
    }

    println(Bar(1, Some("a")) |+| Bar(2, None))
    println(Bar(1, Some("a")) |+| Bar(2, Some("b")))

  }

{
  import cats.syntax.semigroup._ // imports syntax for semigroup

  println(1 |+| 2)
  println(Option(1) |+| Option(2))

}