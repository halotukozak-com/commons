package commons
class ToArrayOfTest extends munit.FunSuite:

  test("empty tuple returns empty array") {
    val result = EmptyTuple.toArrayOf[Int]
    assertEquals(result.toList, List.empty[Int])
  }

  test("single element tuple") {
    val tuple = Tuple1(42)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, List(42))
  }

  test("multiple elements") {
    val tuple = (1, 2, 3)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, List(1, 2, 3))
  }

  test("string elements") {
    val tuple = ("a", "b", "c")
    val result = tuple.toArrayOf[String]
    assertEquals(result.toList, List("a", "b", "c"))
  }

  test("preserves element order") {
    val tuple = (10, 20, 30, 40, 50)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, List(10, 20, 30, 40, 50))
  }

  test("result array has correct length") {
    val tuple = ("x", "y", "z")
    val result = tuple.toArrayOf[String]
    assertEquals(result.length, 3)
  }

  test("supertype target") {
    val tuple = (1, 2, 3)
    val result = tuple.toArrayOf[Any]
    assertEquals(result.toList, List(1, 2, 3))
  }

  test("tail") {
    val tuple = (1, 2, 3)
    val result = tuple.tail.toArrayOf[Int]
    assertEquals(result.toList, List(2, 3))
  }

  test("large tuple (22 elements)") {
    val tuple = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, (1 to 22).toList)
  }

  test("subtype elements collected as supertype") {
    sealed trait Animal
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val tuple = (Dog("Rex"), Cat("Whiskers"), Dog("Buddy"))
    val result = tuple.toArrayOf[Animal]
    assertEquals(result.toList, List(Dog("Rex"), Cat("Whiskers"), Dog("Buddy")))
  }

  test("double elements") {
    val tuple = (1.1, 2.2, 3.3)
    val result = tuple.toArrayOf[Double]
    assertEquals(result.toList, List(1.1, 2.2, 3.3))
  }

  test("boolean elements") {
    val tuple = (true, false, true)
    val result = tuple.toArrayOf[Boolean]
    assertEquals(result.toList, List(true, false, true))
  }

  test("nullable elements") {
    val tuple: (String | Null, String | Null) = ("hello", null)
    val result = tuple.toArrayOf[String | Null]
    assertEquals(result.toList, List("hello", null))
  }

  test("list elements") {
    val tuple = (List(1, 2), List(3, 4), List(5))
    val result = tuple.toArrayOf[List[Int]]
    assertEquals(result.toList, List(List(1, 2), List(3, 4), List(5)))
  }

  test("option elements") {
    val tuple = (Some(1), None, Some(3))
    val result = tuple.toArrayOf[Option[Int]]
    assertEquals(result.toList, List(Some(1), None, Some(3)))
  }

  test("tuple elements") {
    val tuple = ((1, "a"), (2, "b"))
    val result = tuple.toArrayOf[(Int, String)]
    assertEquals(result.toList, List((1, "a"), (2, "b")))
  }

  test("result is a mutable array") {
    val tuple = (1, 2, 3)
    val result = tuple.toArrayOf[Int]
    result(0) = 99
    assertEquals(result(0), 99)
    assertEquals(result(1), 2)
  }

  test("chained tail calls") {
    val tuple = (1, 2, 3, 4, 5)
    val result = tuple.tail.tail.toArrayOf[Int]
    assertEquals(result.toList, List(3, 4, 5))
  }

  test("single element after tail") {
    val tuple = (1, 2)
    val result = tuple.tail.toArrayOf[Int]
    assertEquals(result.toList, List(2))
  }

  test("tail to empty") {
    val tuple = Tuple1(42)
    val result = tuple.tail.toArrayOf[Int]
    assertEquals(result.toList, List.empty[Int])
  }

  test("drop") {
    val tuple = (1, 2, 3, 4, 5)
    val result = tuple.drop(2).toArrayOf[Int]
    assertEquals(result.toList, List(3, 4, 5))
  }

  test("drop all") {
    val tuple = (1, 2, 3)
    val result = tuple.drop(3).toArrayOf[Int]
    assertEquals(result.toList, List.empty[Int])
  }

  test("take") {
    val tuple = (1, 2, 3, 4, 5)
    val result = tuple.take(3).toArrayOf[Int]
    assertEquals(result.toList, List(1, 2, 3))
  }

  test("take zero") {
    val tuple = (1, 2, 3)
    val result = tuple.take(0).toArrayOf[Int]
    assertEquals(result.toList, List.empty[Int])
  }

  test("splitAt first half") {
    val tuple = (1, 2, 3, 4)
    val (first, _) = tuple.splitAt(2)
    val result = first.toArrayOf[Int]
    assertEquals(result.toList, List(1, 2))
  }

  test("splitAt second half") {
    val tuple = (1, 2, 3, 4)
    val (_, second) = tuple.splitAt(2)
    val result = second.toArrayOf[Int]
    assertEquals(result.toList, List(3, 4))
  }

  test("init") {
    val tuple = (1, 2, 3, 4)
    val result = tuple.init.toArrayOf[Int]
    assertEquals(result.toList, List(1, 2, 3))
  }

  test("concat with ++") {
    val a = (1, 2)
    val b = (3, 4)
    val result = (a ++ b).toArrayOf[Int]
    assertEquals(result.toList, List(1, 2, 3, 4))
  }

  test("reverse") {
    val tuple = (1, 2, 3)
    val result = tuple.reverse.toArrayOf[Int]
    assertEquals(result.toList, List(3, 2, 1))
  }

  test("head *: tail roundtrip") {
    val tuple = (1, 2, 3)
    val reconstructed = tuple.head *: tuple.tail
    val result = reconstructed.toArrayOf[Int]
    assertEquals(result.toList, List(1, 2, 3))
  }

  test("drop then take") {
    val tuple = (1, 2, 3, 4, 5)
    val result = tuple.drop(1).take(3).toArrayOf[Int]
    assertEquals(result.toList, List(2, 3, 4))
  }
