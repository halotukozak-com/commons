package halotukozak
package commons

def deepSum(n: Int): Int = deepRecursive:
  if n == 0 then 0
  else 1 + deepSum(n - 1)

// the branch is itself the recursive call, with no surrounding expression (e.g. `1 + ...`)
def deepCountDown(n: Int): Int = deepRecursive:
  if n <= 0 then 0
  else deepCountDown(n - 1)

// two recursive calls within a single branch
def deepFib(n: Int): Int = deepRecursive:
  if n < 2 then n
  else deepFib(n - 1) + deepFib(n - 2)

// more than two recursive calls within a single branch
def deepTribonacci(n: Int): Long = deepRecursive:
  if n < 2 then n.toLong
  else if n == 2 then 1L
  else deepTribonacci(n - 1) + deepTribonacci(n - 2) + deepTribonacci(n - 3)

// recursion via `match` with guards, not `if`
def deepCollatzSteps(n: Long): Int = deepRecursive:
  n match
    case 1L => 0
    case x if x % 2 == 0 => 1 + deepCollatzSteps(x / 2)
    case x => 1 + deepCollatzSteps(3 * x + 1)

// multiple parameters - checks that parameter symbols are substituted in `loop`
def deepSumAcc(n: Int, acc: Long): Long = deepRecursive:
  if n == 0 then acc
  else deepSumAcc(n - 1, acc + n)

// recursive call in the last expression of a block, after a local val
def deepBlockSum(n: Int): Int = deepRecursive:
  if n == 0 then 0
  else
    val prev = n - 1
    1 + deepBlockSum(prev)

// generic type parameter - `loop` must see the enclosing method's type parameter
def deepRepeat[A](n: Int, a: A): A = deepRecursive:
  if n == 0 then a else deepRepeat(n - 1, a)

final case class Bump(amount: Int)

// a trailing `using` clause - the recursive call is a curried, multi-clause Apply
def deepWithBump(n: Int)(using bump: Bump): Int = deepRecursive:
  if n == 0 then 0 else bump.amount + deepWithBump(n - 1)

// a context bound desugars to its own synthesized `using` clause, exercising the same
// multi-clause flattening as an explicit `using` clause, plus evidence use inside the loop
def deepSumWith[A: Numeric](n: Int, a: A): A = deepRecursive:
  if n == 0 then a
  else deepSumWith(n - 1, Numeric[A].plus(a, Numeric[A].one))

final case class Step(amount: Int)

// an old-style `implicit` clause (as opposed to `using`) - same multi-clause shape
def deepImplicitStep(n: Int)(implicit step: Step): Int = deepRecursive:
  if n == 0 then 0 else step.amount + deepImplicitStep(n - 1)

// plain (non-macro) reference implementations to compare results against
def plainFib(n: Int): Int =
  if n < 2 then n else plainFib(n - 1) + plainFib(n - 2)

def plainTribonacci(n: Int): Long =
  if n < 2 then n.toLong
  else if n == 2 then 1L
  else plainTribonacci(n - 1) + plainTribonacci(n - 2) + plainTribonacci(n - 3)

def plainCollatzSteps(n: Long): Int =
  if n == 1L then 0
  else if n % 2 == 0 then 1 + plainCollatzSteps(n / 2)
  else 1 + plainCollatzSteps(3 * n + 1)

class DeepRecursiveFunctionTest extends munit.FunSuite:

  test("computes the correct result for shallow recursion with one call per branch") {
    assertEquals(deepSum(0), 0)
    assertEquals(deepSum(1), 1)
    assertEquals(deepSum(10), 10)
  }

  test("is stack-safe for deep recursion with one call per branch") {
    assertEquals(deepSum(1_000_000), 1_000_000)
  }

  test("works when the whole branch is the recursive call itself (no wrapper)") {
    assertEquals(deepCountDown(0), 0)
    assertEquals(deepCountDown(1_000_000), 0)
  }

  test("supports two recursive calls within a single branch (fib)") {
    (0 to 20).foreach(n => assertEquals(deepFib(n), plainFib(n), s"n = $n"))
  }

  test("supports more than two recursive calls within a single branch (tribonacci)") {
    (0 to 20).foreach(n => assertEquals(deepTribonacci(n), plainTribonacci(n), s"n = $n"))
  }

  test("supports recursion via match with guards") {
    Seq(1L, 2L, 6L, 7L, 27L, 97L).foreach(n => assertEquals(deepCollatzSteps(n), plainCollatzSteps(n), s"n = $n"))
  }

  test("correctly substitutes parameter symbols with multiple arguments") {
    assertEquals(deepSumAcc(100, 0L), (1 to 100).map(_.toLong).sum)
    assertEquals(deepSumAcc(0, 42L), 42L)
  }

  test("supports a recursive call inside a block, after a local val") {
    assertEquals(deepBlockSum(0), 0)
    assertEquals(deepBlockSum(1_000_000), 1_000_000)
  }

  test("supports a generic type parameter on the enclosing method") {
    assertEquals(deepRepeat(0, "x"), "x")
    assertEquals(deepRepeat(5, "x"), "x")
    assertEquals(deepRepeat(5, 42), 42)
  }

  test("supports a trailing using clause on the enclosing method") {
    given Bump = Bump(3)
    assertEquals(deepWithBump(0), 0)
    assertEquals(deepWithBump(5), 15)
  }

  test("supports a context bound on the enclosing method's type parameter") {
    assertEquals(deepSumWith(0, 0), 0)
    assertEquals(deepSumWith(5, 0), 5)
    assertEquals(deepSumWith(5, 10L), 15L)
  }

  test("supports an old-style implicit clause on the enclosing method") {
    implicit val step: Step = Step(2)
    assertEquals(deepImplicitStep(0), 0)
    assertEquals(deepImplicitStep(5), 10)
  }

  test("rejects a recursive call nested inside a closure instead of silently changing its call count") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else List(1, 2, 3).map(_ => f(n - 1)).sum }""",
    )
    assert(res.contains("cannot safely trampoline"), res)
  }

  test("rejects a recursive call nested inside a try instead of silently looping forever") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { try { if n == 0 then 0 else 1 + f(n - 1) } catch case _: Exception => -1 } """,
    )
    assert(res.contains("cannot safely trampoline"), res)
  }

  test("rejects a recursive call bound to a val before the tail expression instead of silently leaving it non-trampolined") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else { val prev = f(n - 1); 1 + prev } }""",
    )
    assert(res.contains("final expression"), res)
  }

  test("rejects deepRecursive used inside a lambda instead of silently skipping the trampoline") {
    val res = compileErrors(
      """lazy val f: Int => Int = n => deepRecursive { if n == 0 then 0 else 1 + f(n - 1) }""",
    )
    assert(res.contains("body of a named"), res)
  }
