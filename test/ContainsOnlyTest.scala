package halotukozak.commons

import halotukozak.commons.containsOnly.given

class ContainsOnlyTest extends munit.FunSuite:

  // --- Loop type-level: positive cases ---

  test("Loop: EmptyTuple containsOnly Int") {
    summon[containsOnly.Loop[EmptyTuple, Int] =:= true]
  }

  test("Loop: EmptyTuple containsOnly String") {
    summon[containsOnly.Loop[EmptyTuple, String] =:= true]
  }

  test("Loop: single Int containsOnly Int") {
    summon[containsOnly.Loop[Tuple1[Int], Int] =:= true]
  }

  test("Loop: single String containsOnly String") {
    summon[containsOnly.Loop[Tuple1[String], String] =:= true]
  }

  test("Loop: (Int, Int) containsOnly Int") {
    summon[containsOnly.Loop[(Int, Int), Int] =:= true]
  }

  test("Loop: (Int, Int, Int) containsOnly Int") {
    summon[containsOnly.Loop[(Int, Int, Int), Int] =:= true]
  }

  test("Loop: (String, String) containsOnly String") {
    summon[containsOnly.Loop[(String, String), String] =:= true]
  }

  test("Loop: (Boolean, Boolean, Boolean) containsOnly Boolean") {
    summon[containsOnly.Loop[(Boolean, Boolean, Boolean), Boolean] =:= true]
  }

  test("Loop: (Double, Double) containsOnly Double") {
    summon[containsOnly.Loop[(Double, Double), Double] =:= true]
  }

  // --- Loop type-level: negative cases ---

  test("Loop: (Int, String) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, String), Int] =:= false]
  }

  test("Loop: (String, Int) does not containOnly String") {
    summon[containsOnly.Loop[(String, Int), String] =:= false]
  }

  test("Loop: (Int, Int, String) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, Int, String), Int] =:= false]
  }

  test("Loop: (String, Int, String) does not containOnly String") {
    summon[containsOnly.Loop[(String, Int, String), String] =:= false]
  }

  test("Loop: (Int, Double) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, Double), Int] =:= false]
  }

  test("Loop: single Int does not containOnly String") {
    summon[containsOnly.Loop[Tuple1[Int], String] =:= false]
  }

  // --- Loop type-level: subtype behavior ---

  test("Loop: subtypes with sealed trait") {
    sealed trait Fruit
    case class Apple() extends Fruit
    case class Banana() extends Fruit

    summon[containsOnly.Loop[(Apple, Banana), Fruit] =:= true]
  }

  test("Loop: single subtype with sealed trait") {
    sealed trait Fruit
    case class Apple() extends Fruit

    summon[containsOnly.Loop[Tuple1[Apple], Fruit] =:= true]
  }

  test("Loop: mixed subtype and unrelated type") {
    sealed trait Fruit
    case class Apple() extends Fruit

    summon[containsOnly.Loop[(Apple, String), Fruit] =:= false]
  }

  // --- given instance: evidence is summoned correctly ---

  test("given: evidence available for EmptyTuple") {
    summon[EmptyTuple containsOnly Int]
  }

  test("given: evidence available for (Int, Int, Int)") {
    summon[(Int, Int, Int) containsOnly Int]
  }

  test("given: evidence available for (String, String)") {
    summon[(String, String) containsOnly String]
  }

  test("given: evidence available for single element") {
    summon[Tuple1[Int] containsOnly Int]
  }

  test("given: no evidence for heterogeneous tuple") {
    val errors = compileErrors("summon[(Int, String) containsOnly Int]")
    assert(errors.nonEmpty, "Should not find evidence for heterogeneous tuple")
  }

  test("given: no evidence for wrong type") {
    val errors = compileErrors("summon[(Int, Int) containsOnly String]")
    assert(errors.nonEmpty, "Should not find evidence for wrong type")
  }

  // --- integration with mapAs ---

  test("mapAs compiles for homogeneous Int tuple") {
    val result = (1, 2, 3).mapAs[Int]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(1), List(2), List(3)))
  }

  test("mapAs compiles for homogeneous String tuple") {
    val result = ("a", "b").mapAs[String]([t <: String] => (x: t) => Option(x))
    assertEquals(result, (Some("a"), Some("b")))
  }

  test("mapAs compiles for empty tuple") {
    val result = EmptyTuple.mapAs[Int]([t <: Int] => (x: t) => List(x))
    assertEquals(result, EmptyTuple)
  }

  test("mapAs compiles for single element") {
    val result = Tuple1(42).mapAs[Int]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, Tuple1(Some(42)))
  }

  test("mapAs compiles for subtype hierarchy") {
    sealed trait Animal
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val result = (Dog("Rex"), Cat("Whiskers")).mapAs[Animal]([t <: Animal] => (x: t) => Option(x))
    assertEquals(result, (Some(Dog("Rex")), Some(Cat("Whiskers"))))
  }

  // --- does not compile cases ---

  test("does not compile: heterogeneous tuple with mapAs") {
    val errors = compileErrors("""
      (1, "a").mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: wrong target type with mapAs") {
    val errors = compileErrors("""
      (1, 2, 3).mapAs[String]([t <: String] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: Int and Double mix") {
    val errors = compileErrors("""
      (1, 2.0).mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: Boolean among Ints") {
    val errors = compileErrors("""
      (1, true, 2).mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: String among Ints at end") {
    val errors = compileErrors("""
      (1, 2, "three").mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: String among Ints at start") {
    val errors = compileErrors("""
      ("zero", 1, 2).mapAs[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("evidence allows head access") {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    val first: Int = tuple.head
    assertEquals(first, 1)
  }

  test("evidence allows last access") {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    val last: Int = tuple.last
    assertEquals(last, 3)
  }

  // `containsOnly` provides Head/Last/Elem → T conversions today. Operations like
  // `drop`, `take`, `tail`, `init`, `reverse`, `mapAs`, `toList`, `++`
  // need the static tuple shape to reduce match types, which we don't carry on an
  // abstract `Tuple` value. Coverage for those would require a richer evidence type.

  test("evidence on single element") {
    val tuple: Tuple = Tuple1(42)
    given tuple.type containsOnly Int = containsOnly.refl

    val first: Int = tuple.head
    assertEquals(first, 42)
  }

  test("evidence with String tuple") {
    val tuple: Tuple = ("a", "b", "c")
    given tuple.type containsOnly String = containsOnly.refl

    val first: String = tuple.head
    assertEquals(first, "a")
    assertEquals(tuple.size, 3)
  }

  test("containsOnly is contravariant in Tup") {
    sealed trait Fruit

    class Apple extends Fruit

    class Banana extends Fruit

    summon[(Fruit, Fruit) containsOnly Fruit]
    // Since (Apple, Apple) <: (Fruit, Fruit)
    // and containsOnly is -Tup
    // then containsOnly[(Fruit, Fruit), Fruit] <: containsOnly[(Apple, Apple), Fruit]
    summon[(Apple, Apple) containsOnly Fruit]

    type Super
    type A <: Super
    type B <: Super

    summon[(A, A) containsOnly Super]
    summon[(A, B) containsOnly Super]
  }

  test("containsOnly is covariant in T: singleton widens to Int") {
    val tuple: Tuple = (3, 3, 3)
    given (tuple.type containsOnly 3) = containsOnly.refl

    summon[tuple.type containsOnly Int]
  }

  test("containsOnly is covariant in T: subclass widens to superclass") {
    sealed trait Fruit
    class Apple extends Fruit

    val tuple: Tuple = ("a", "b")

    given (tuple.type containsOnly Apple) = containsOnly.refl

    summon[tuple.type containsOnly Fruit]
  }

  test("containsOnly is covariant in T: abstract type bound widens") {
    type Super
    type A <: Super

    val tuple: Tuple = ("a", "b")

    given (tuple.type containsOnly A) = containsOnly.refl

    summon[tuple.type containsOnly Super]
  }

  test("containsOnly is covariant in T: String literal widens to String") {
    val tuple: Tuple = ("a", "b")
    given (tuple.type containsOnly "a") = containsOnly.refl

    summon[tuple.type containsOnly String]
  }

  test("any tuple containsOnly Any without explicit evidence") {
    val tuple = ("a", "b")

    summon[tuple.type containsOnly Any]
  }

  test("heterogeneous tuple containsOnly Any") {
    summon[(Int, String, Boolean) containsOnly Any]
  }

  test("abstract Tuple containsOnly Any") {
    val tuple: Tuple = (1, "x", true)

    summon[tuple.type containsOnly Any]
  }

  test("EmptyTuple containsOnly Any") {
    summon[EmptyTuple containsOnly Any]
  }

  test("nested tuple containsOnly Any") {
    summon[((Int, Int), (String, String)) containsOnly Any]
  }

  test("Tuple.Concat preserves containsOnly") {
    val tuple1 = ("one", "two", "three")
    val tuple2 = ("four", "five")

    summon[Tuple.Concat[tuple1.type, tuple2.type] containsOnly String]

    import Tuple.++
    summon[(tuple1.type ++ tuple2.type) containsOnly String]
  }

  test("Tuple.Tail preserves containsOnly") {
    val tuple = ("one", "two", "three")

    summon[Tuple.Tail[tuple.type] containsOnly String]
  }

  test("Tuple.Reverse preserves containsOnly") {
    val tuple = ("one", "two", "three")

    summon[Tuple.Reverse[tuple.type] containsOnly String]
  }

  test("Tuple.Elem gives evidence at an arbitrary index") {
    val tuple: Tuple = ("one", "two", "three")
    given tuple.type containsOnly String = containsOnly.refl

    // N=0 is deliberately not tested here: it collides with the dedicated Head evidence
    // (both reduce to the same type), which is exactly why Elem is given lower priority.
    summon[Tuple.Elem[tuple.type, 1] <:< String]
    summon[Tuple.Elem[tuple.type, 2] <:< String]
  }

  test("Tuple.Init preserves containsOnly") {
    val tuple = ("one", "two", "three")

    summon[Tuple.Init[tuple.type] containsOnly String]
  }

  test("Tuple.Take preserves containsOnly") {
    val tuple = ("one", "two", "three")

    summon[Tuple.Take[tuple.type, 0] containsOnly String]
    summon[Tuple.Take[tuple.type, 2] containsOnly String]
    summon[Tuple.Take[tuple.type, 3] containsOnly String]
  }

  test("Tuple.Drop preserves containsOnly") {
    val tuple = ("one", "two", "three")

    summon[Tuple.Drop[tuple.type, 0] containsOnly String]
    summon[Tuple.Drop[tuple.type, 1] containsOnly String]
    summon[Tuple.Drop[tuple.type, 3] containsOnly String]
  }

  test("Tuple.Filter preserves containsOnly") {
    val tuple = ("one", "two", "three")

    type IsString[X] <: Boolean = X match
      case String => true
      case _ => false

    summon[Tuple.Filter[tuple.type, IsString] containsOnly String]
  }

  test("Tuple.Filter preserves containsOnly for an abstract Tuple") {
    val tuple: Tuple = ("one", "two", "three")
    given tuple.type containsOnly String = containsOnly.refl

    type IsString[X] <: Boolean = X match
      case String => true
      case _ => false

    summon[Tuple.Filter[tuple.type, IsString] containsOnly String]
  }

  test("Tuple.Union gives evidence for the union of all elements") {
    val tuple: Tuple = ("one", "two", "three")
    given tuple.type containsOnly String = containsOnly.refl

    summon[Tuple.Union[tuple.type] <:< String]
  }

  test("Tuple.Append preserves containsOnly") {
    val tuple = ("one", "two", "three")

    summon[Tuple.Append[tuple.type, "four"] containsOnly String]

    import Tuple.:*
    summon[(tuple.type :* "four") containsOnly String]
  }

  test("Tuple.Append preserves containsOnly for an abstract Tuple") {
    val tuple: Tuple = ("one", "two", "three")
    given tuple.type containsOnly String = containsOnly.refl

    summon[Tuple.Append[tuple.type, "four"] containsOnly String]
  }

  test("Tuple.Map with a constant function preserves containsOnly (already supported)") {
    type Es = (Int, String, Boolean)

    summon[Tuple.Map[Es, [_] =>> List[Int]] containsOnly List[Int]]
  }

  test("Tuple.Map with a covariant type constructor preserves containsOnly (already supported)") {
    val tuple = (1, 2, 3)

    summon[Tuple.Map[tuple.type, Option] containsOnly Option[Any]]
  }

  test(
    "Tuple.Fold has no blanket containsOnly given: it depends entirely on F, so only Union (a specific Fold) is covered",
  ) {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    // Union[Tup] = Fold[Tup, Nothing, [x, y] =>> x | y] — this specific instantiation is covered.
    summon[Tuple.Union[tuple.type] <:< Int]

    // A generic Fold isn't: nothing constrains F to preserve "all elements are T" (F could
    // produce a String out of Ints, e.g. `[x, y] =>> String`), so no sound blanket given can exist.
  }

  test("Tuple.Split preserves containsOnly via the existing Take/Drop givens") {
    val tuple = ("one", "two", "three", "four")

    summon[Tuple.Take[tuple.type, 2] containsOnly String]
    summon[Tuple.Drop[tuple.type, 2] containsOnly String]

    // Tuple.Split[Tup, N] = (Take[Tup, N], Drop[Tup, N]) — a 2-tuple of tuples, not itself
    // homogeneous in String, so it needs no dedicated given; each half is already covered.
    val split: Tuple.Split[tuple.type, 2] = (("one", "two"), ("three", "four"))
    val (taken, dropped) = split
    summon[taken.type containsOnly String]
    summon[dropped.type containsOnly String]
  }
