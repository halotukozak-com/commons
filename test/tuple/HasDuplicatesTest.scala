package commons
class HasDuplicatesTest extends munit.FunSuite:

  // --- runtime tests ---

  test("empty tuple has no duplicates") {
    val result = EmptyTuple.hasDuplicates
    assertEquals(result, false)
  }

  test("single element has no duplicates") {
    val result = Tuple1(1).hasDuplicates
    assertEquals(result, false)
  }

  test("two elements of same type have duplicates") {
    val result = (1, 2).hasDuplicates
    assertEquals(result, true)
  }

  test("two elements of different types have no duplicates") {
    val result = (1, "a").hasDuplicates
    assertEquals(result, false)
  }

  test("duplicate type at the end") {
    val result = (1, "a", 2).hasDuplicates
    assertEquals(result, true)
  }

  test("all same type elements") {
    val result = (1, 2, 3).hasDuplicates
    assertEquals(result, true)
  }

  test("no duplicate types in longer heterogeneous tuple") {
    val result = (1, "a", true, 2.0, 'c').hasDuplicates
    assertEquals(result, false)
  }

  test("heterogeneous tuple with no type duplicates") {
    val result = (1, "a", true).hasDuplicates
    assertEquals(result, false)
  }

  test("string pair has duplicates") {
    val result = ("a", "b").hasDuplicates
    assertEquals(result, true)
  }

  test("boolean pair has duplicates") {
    val result = (true, false).hasDuplicates
    assertEquals(result, true)
  }

  test("duplicate type only at start and end") {
    val result = (1, "a", true, 2).hasDuplicates
    assertEquals(result, true)
  }

  // --- type-level tests ---

  test("HasDuplicates of EmptyTuple is false") {
    summon[HasDuplicates[EmptyTuple] =:= false]
  }

  test("HasDuplicates of single element is false") {
    summon[HasDuplicates[Tuple1[Int]] =:= false]
  }

  test("HasDuplicates of (Int, Int) is true") {
    summon[HasDuplicates[(Int, Int)] =:= true]
  }

  test("HasDuplicates of (Int, String) is false") {
    summon[HasDuplicates[(Int, String)] =:= false]
  }

  test("HasDuplicates of (Int, String, Int) is true") {
    summon[HasDuplicates[(Int, String, Int)] =:= true]
  }

  test("HasDuplicates of (Int, String, Double) is false") {
    summon[HasDuplicates[(Int, String, Double)] =:= false]
  }

  test("HasDuplicates detects duplicate at end") {
    summon[HasDuplicates[(String, Int, String)] =:= true]
  }

  test("HasDuplicates of all same type is true") {
    summon[HasDuplicates[(Int, Int, Int)] =:= true]
  }
