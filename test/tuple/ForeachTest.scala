package commons
class ForeachTest extends munit.FunSuite:

  test("empty tuple does nothing") {
    var count = 0
    EmptyTuple.foreach([t] => (_: t) => count += 1)
    assertEquals(count, 0)
  }

  test("visits every element") {
    var count = 0
    (1, 2, 3).foreach([t] => (_: t) => count += 1)
    assertEquals(count, 3)
  }

  test("visits elements in order") {
    val builder = List.newBuilder[Any]
    ("a", "b", "c").foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List("a", "b", "c"))
  }

  test("single element tuple") {
    val builder = List.newBuilder[Any]
    Tuple1(42).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(42))
  }

  test("heterogeneous tuple") {
    val builder = List.newBuilder[Any]
    (1, "two", 3.0, true).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(1, "two", 3.0, true))
  }

  test("side effects") {
    var sum = 0
    (10, 20, 30).foreach([t] => (x: t) => sum += x.asInstanceOf[Int])
    assertEquals(sum, 60)
  }

  test("two element tuple") {
    val builder = List.newBuilder[Any]
    (1, 2).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(1, 2))
  }

  test("foreach with string concatenation side effect") {
    var result = ""
    ("a", "b", "c").foreach([t] => (x: t) => result += x.toString)
    assertEquals(result, "abc")
  }

  test("foreach on large tuple") {
    var count = 0
    (1, 2, 3, 4, 5, 6, 7, 8, 9, 10).foreach([t] => (_: t) => count += 1)
    assertEquals(count, 10)
  }

  test("foreach preserves element identity") {
    val obj1 = new Object
    val obj2 = new Object
    val seen = scala.collection.mutable.ListBuffer[AnyRef]()
    (obj1, obj2).foreach([t] => (x: t) => seen += x.asInstanceOf[AnyRef])
    assert(seen(0) eq obj1)
    assert(seen(1) eq obj2)
  }

  test("foreach with null elements") {
    val builder = List.newBuilder[Any]
    (null, "a", null).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(null, "a", null))
  }

  test("foreach collects types correctly for homogeneous tuple") {
    val ints = scala.collection.mutable.ListBuffer[Int]()
    (1, 2, 3).foreach([t] => (x: t) => ints += x.asInstanceOf[Int])
    assertEquals(ints.toList, List(1, 2, 3))
  }

  test("nested foreach") {
    val builder = List.newBuilder[Any]
    val inner = (3, 4)
    (1, 2).foreach { [t] => (x: t) =>
      builder += x
      inner.foreach([u] => (y: u) => builder += y)
    }
    assertEquals(builder.result(), List(1, 3, 4, 2, 3, 4))
  }
