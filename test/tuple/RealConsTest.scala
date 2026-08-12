package commons

class RealConsTest extends munit.FunSuite:

  test("prepends the value at runtime like *:") {
    val res: Tuple = realCons(1, (2, 3))
    assertEquals(res, (1, 2, 3))
  }

  test("prepends onto an empty tuple") {
    val res: Tuple = realCons(1, EmptyTuple)
    assertEquals(res, Tuple1(1))
  }

  // *: only infers a precise singleton-typed result when an expected type happens to propagate
  // into the call (e.g. an immediate `val r: 5 *: (1, 2) = x *: tup` ascription). Without that,
  // its type parameters are inferred from the *static* type of the arguments alone and widen (5
  // becomes Int). realCons's result type is fixed to `x.type *: tup.type` by its signature, and
  // being inline, `x.type`/`tup.type` resolve to the singleton type of the actual argument
  // expressions unconditionally — so the precise type survives even without any expected type.
  test("keeps the precise singleton type without an expected type, unlike *:") {
    val x: 5 = 5
    val tup: (1, 2) = (1, 2)

    val real = realCons(x, tup)
    val realPrecise: 5 *: (1, 2) = real
    assertEquals(realPrecise, (5, 1, 2))

    val res = compileErrors(
      """{ val x: 5 = 5; val tup: (1, 2) = (1, 2); val consed = x *: tup; val widened: 5 *: (1, 2) = consed }""",
    )
    assert(res.nonEmpty)
  }

  test("preserves a String literal's singleton type without an expected type, unlike *:") {
    val x: "a" = "a"
    val tup: ("b", "c") = ("b", "c")

    val real = realCons(x, tup)
    val realPrecise: "a" *: ("b", "c") = real
    assertEquals(realPrecise, ("a", "b", "c"))

    val res = compileErrors(
      """{ val x: "a" = "a"; val tup: ("b", "c") = ("b", "c"); val consed = x *: tup; val widened: "a" *: ("b", "c") = consed }""",
    )
    assert(res.nonEmpty)
  }
