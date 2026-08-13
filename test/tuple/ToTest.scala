package halotukozak.commons

class ToTest extends munit.FunSuite:

  test("empty tuple to List") {
    val result = EmptyTuple.to[Int](List)
    assertEquals(result, List.empty[Int])
  }

  test("single element tuple to List") {
    val tuple = Tuple1(42)
    val result = tuple.to[Int](List)
    assertEquals(result, List(42))
  }

  test("multiple elements to List") {
    val tuple = (1, 2, 3)
    val result = tuple.to[Int](List)
    assertEquals(result, List(1, 2, 3))
  }

  test("string elements to List") {
    val tuple = ("a", "b", "c")
    val result = tuple.to[String](List)
    assertEquals(result, List("a", "b", "c"))
  }

  test("preserves element order") {
    val tuple = (10, 20, 30, 40, 50)
    val result = tuple.to[Int](List)
    assertEquals(result, List(10, 20, 30, 40, 50))
  }

  test("supertype target") {
    val tuple = (1, 2, 3)
    val result = tuple.to[Any](List)
    assertEquals(result, List(1, 2, 3))
  }

  test("tail") {
    val tuple = (1, 2, 3)
    val result = tuple.tail.to[Int](List)
    assertEquals(result, List(2, 3))
  }

  test("large tuple (22 elements)") {
    val tuple = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val result = tuple.to[Int](List)
    assertEquals(result, (1 to 22).toList)
  }

  test("subtype elements collected as supertype") {
    sealed trait Animal
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val tuple = (Dog("Rex"), Cat("Whiskers"), Dog("Buddy"))
    val result = tuple.to[Animal](List)
    assertEquals(result, List(Dog("Rex"), Cat("Whiskers"), Dog("Buddy")))
  }

  test("nullable elements") {
    val tuple: (String | Null, String | Null) = ("hello", null)
    val result = tuple.to[String | Null](List)
    assertEquals(result, List("hello", null))
  }

  test("to Vector") {
    val tuple = (1, 2, 3)
    val result = tuple.to[Int](Vector)
    assertEquals(result, Vector(1, 2, 3))
  }

  test("to Set deduplicates") {
    val tuple = (1, 2, 2, 3, 1)
    val result = tuple.to[Int](Set)
    assertEquals(result, Set(1, 2, 3))
  }

  test("to Map from tuple of pairs") {
    val tuple = (("a", 1), ("b", 2), ("c", 3))
    val result = tuple.to[(String, Int)](Map)
    assertEquals(result, Map("a" -> 1, "b" -> 2, "c" -> 3))
  }

  test("empty tuple to Set") {
    val result = EmptyTuple.to[Int](Set)
    assertEquals(result, Set.empty[Int])
  }

  test("drop then to List") {
    val tuple = (1, 2, 3, 4, 5)
    val result = tuple.drop(2).to[Int](List)
    assertEquals(result, List(3, 4, 5))
  }

  test("take then to List") {
    val tuple = (1, 2, 3, 4, 5)
    val result = tuple.take(3).to[Int](List)
    assertEquals(result, List(1, 2, 3))
  }

  test("reverse then to List") {
    val tuple = (1, 2, 3)
    val result = tuple.reverse.to[Int](List)
    assertEquals(result, List(3, 2, 1))
  }

  test("concat with ++ then to List") {
    val a = (1, 2)
    val b = (3, 4)
    val result = (a ++ b).to[Int](List)
    assertEquals(result, List(1, 2, 3, 4))
  }
