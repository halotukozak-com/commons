package halotukozak.commons

class OfRefinedTupleTest extends munit.FunSuite:

  test("builds a tuple with the right values and order") {
    assertEquals(OfRefinedTupleFixture.triple, (1, "a", true))
  }

  test("keeps each element's precise (constant) type") {
    val precise: (1, "a", true) = OfRefinedTupleFixture.triple
    assertEquals(precise, (1, "a", true))
  }

  test("an empty list builds EmptyTuple") {
    assertEquals(OfRefinedTupleFixture.empty, EmptyTuple)
  }
