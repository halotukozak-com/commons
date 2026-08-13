package halotukozak.commons
class IndicesTest extends munit.FunSuite:

  // --- runtime tests ---

  test("empty tuple") {
    val result = EmptyTuple.indices
    assertEquals(result, EmptyTuple)
  }

  test("single element tuple") {
    val result = Tuple1("a").indices
    assertEquals(result, Tuple1(0))
  }

  test("three element tuple") {
    val result = (10, 20, 30).indices
    assertEquals(result, (0, 1, 2))
  }

  test("five element tuple") {
    val result = ("a", "b", "c", "d", "e").indices
    assertEquals(result, (0, 1, 2, 3, 4))
  }

  test("size matches tuple size") {
    val tuple = (1, 2, 3, 4)
    assertEquals(tuple.indices.size, tuple.size)
  }

  test("heterogeneous tuple") {
    val result = (1, "two", 3.0).indices
    assertEquals(result, (0, 1, 2))
  }

  test("two element tuple") {
    val result = (true, false).indices
    assertEquals(result, (0, 1))
  }

  test("indices values are sequential starting from 0") {
    val result = ("a", "b", "c", "d").indices
    val asList = result.toList
    assertEquals(asList, List(0, 1, 2, 3))
  }

  test("indices of large tuple") {
    val result = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10).indices
    assertEquals(result, (0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("indices result has same arity as input") {
    val tuple = ("a", "b", "c")
    assertEquals(tuple.indices.size, 3)
  }

  test("indices of homogeneous tuple") {
    val result = (1, 2, 3).indices
    assertEquals(result, (0, 1, 2))
  }

  // --- type-level tests ---

  test("Indices of EmptyTuple is EmptyTuple") {
    summon[Indices[EmptyTuple] =:= EmptyTuple]
  }

  test("Indices of single element tuple") {
    summon[Indices[String *: EmptyTuple] =:= (0 *: EmptyTuple)]
  }

  test("Indices of three element tuple") {
    summon[Indices[(Int, String, Double)] <:< (0, 1, 2)]
  }

  test("Indices of five element tuple") {
    summon[Indices[(Any, Any, Any, Any, Any)] <:< (0, 1, 2, 3, 4)]
  }

  test("Indices is a subtype of Tuple") {
    summon[Indices[(Int, String)] <:< Tuple]
  }

  test("Indices element types are literal Int singletons") {
    summon[Indices[(String, Boolean, Double)] <:< (0, 1, 2)]
    summon[(0, 1, 2) <:< Indices[(String, Boolean, Double)]]
  }

  test("Indices of two element tuple at type level") {
    summon[Indices[(Int, String)] <:< (0, 1)]
    summon[(0, 1) <:< Indices[(Int, String)]]
  }

  test("IndicesAux starts from given offset") {
    summon[IndicesAux[(String, Int), 3] <:< (3, 4)]
  }

  test("IndicesAux of EmptyTuple is EmptyTuple") {
    summon[IndicesAux[EmptyTuple, 5] =:= EmptyTuple]
  }
