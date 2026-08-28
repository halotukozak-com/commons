package halotukozak
package commons

object DeepRecursiveFunctionTest:
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

  // the recursive call itself is bound to a `val` before the tail expression
  def deepValBound(n: Int): Int = deepRecursive:
    if n == 0 then 0
    else
      val prev = deepValBound(n - 1)
      1 + prev

  // multiple `val`s, each binding a recursive call, chained before the tail expression
  def deepChainedVals(n: Int): Int = deepRecursive:
    if n <= 1 then n
    else
      val a = deepChainedVals(n - 1)
      val b = deepChainedVals(n - 2)
      a + b

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

  enum Tree:
    case Leaf(n: Int)
    case Node(children: List[Tree])

  // self-call threaded unchanged through `.map` (bare method reference, i.e. eta-expansion) -
  // recognized as a stack-safe traversal instead of being rejected as an unsafe closure
  def deepSumTree(t: Tree): Int = deepRecursive:
    t match
      case Tree.Leaf(n) => n
      case Tree.Node(children) => children.map(deepSumTree).sum

  // same shape, but with an explicit lambda instead of a bare method reference
  def deepSumTreeLambda(t: Tree): Int = deepRecursive:
    t match
      case Tree.Leaf(n) => n
      case Tree.Node(children) => children.map(c => deepSumTreeLambda(c)).sum

  // the .map closure does more than thread the element through unchanged - it wraps the
  // self-call's own result in extra computation (`+ 1`), reusing the same tail-position
  // extraction/substitution machinery that already supports e.g. `1 + deepSum(n - 1)`
  def deepSumTreePlusOne(t: Tree): Int = deepRecursive:
    t match
      case Tree.Leaf(n) => n
      case Tree.Node(children) => children.map(c => deepSumTreePlusOne(c) + 1).sum

  // the .map closure ignores its own element entirely and calls back into an outer-scope
  // self-call instead - each list element triggers its own independent recursive computation
  def deepRepeatedCall(n: Int): Int = deepRecursive:
    if n == 0 then 1 else List(1, 2, 3).map(_ => deepRepeatedCall(n - 1)).sum

  enum TreeV:
    case Leaf(n: Int)
    case Node(children: Vector[TreeV])

  // same shape, but the collection being traversed is a Vector instead of a List
  def deepSumTreeVector(t: TreeV): Int = deepRecursive:
    t match
      case TreeV.Leaf(n) => n
      case TreeV.Node(children) => children.map(deepSumTreeVector).sum

  enum TreeS:
    case Leaf(n: Int)
    case Node(children: Set[TreeS])

  // same shape, but the collection being traversed is a Set instead of a List
  def deepSumTreeSet(t: TreeS): Int = deepRecursive:
    t match
      case TreeS.Leaf(n) => n
      case TreeS.Node(children) => children.map(deepSumTreeSet).sum

  final case class Wrapped(n: Int, inner: Option[Wrapped])

  // self-call threaded through `.map` over an `Option` instead of a collection - `Option`
  // isn't part of the `Iterable`/`Factory` hierarchy, so this exercises the dedicated
  // `traverseTailRecOption` path rather than the `Factory`-based one
  def deepSumOption(w: Wrapped): Int = deepRecursive:
    w.n + w.inner.map(deepSumOption).getOrElse(0)

  final case class Chained(n: Int, next: Either[String, Chained])

  // same idea, but through the right-biased `Either` instead of `Option`
  def deepSumEither(c: Chained): Int = deepRecursive:
    c.n + c.next.map(deepSumEither).getOrElse(0)

  // memoized variant of the List/.map case - exercises the same MapSelect recognition and
  // buildChain path, but wrapped in `getOrElseUpdate` instead of plain trampolining
  def deepSumTreeMemoized(t: Tree): Int = deepRecursiveMemoized:
    t match
      case Tree.Leaf(n) => n
      case Tree.Node(children) => children.map(deepSumTreeMemoized).sum

  enum Rich:
    case Leaf(n: Int)
    case Node(
      extra: Rich,
      listKids: List[Rich],
      vecKids: Vector[Rich],
      optKid: Option[Rich],
      eitherKid: Either[String, Rich],
    )

  // a single branch mixing a direct self-call with three differently-shaped `.map`-threaded
  // self-calls (List, Vector, Option) plus an Either one - exercises buildChain threading a
  // long, heterogeneous chain of nodes in source order within one branch
  def deepSumRich(r: Rich): Int = deepRecursive:
    r match
      case Rich.Leaf(n) => n
      case Rich.Node(extra, listKids, vecKids, optKid, eitherKid) =>
        deepSumRich(extra) + listKids.map(deepSumRich).sum + vecKids.map(deepSumRich).sum +
          optKid.map(deepSumRich).getOrElse(0) + eitherKid.map(deepSumRich).getOrElse(0)

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
  import DeepRecursiveFunctionTest.*

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

  test("supports a recursive call bound to a val before the tail expression") {
    assertEquals(deepValBound(0), 0)
    assertEquals(deepValBound(10), 10)
  }

  test("is stack-safe for a recursive call bound to a val before the tail expression") {
    assertEquals(deepValBound(1_000_000), 1_000_000)
  }

  test("supports multiple vals, each binding a recursive call, chained before the tail expression") {
    (0 to 20).foreach(n => assertEquals(deepChainedVals(n), plainFib(n), s"n = $n"))
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

  test("supports a self-call threaded through .map over a List (bare method reference)") {
    assertEquals(deepSumTree(Tree.Leaf(5)), 5)
    assertEquals(deepSumTree(Tree.Node(List(Tree.Leaf(1), Tree.Leaf(2), Tree.Leaf(3)))), 6)
    assertEquals(deepSumTree(Tree.Node(List(Tree.Node(List(Tree.Leaf(1), Tree.Leaf(2))), Tree.Leaf(3)))), 6)
  }

  test("supports a self-call threaded through .map over a List (explicit lambda)") {
    assertEquals(deepSumTreeLambda(Tree.Node(List(Tree.Leaf(1), Tree.Leaf(2), Tree.Leaf(3)))), 6)
  }

  test("is stack-safe both for deep nesting through .map and for a wide list of children") {
    val deepChain = (1 to 1_000_000).foldLeft(Tree.Leaf(1): Tree)((acc, _) => Tree.Node(List(acc)))
    assertEquals(deepSumTree(deepChain), 1)

    val wideList = Tree.Node(List.fill(1_000_000)(Tree.Leaf(1)))
    assertEquals(deepSumTree(wideList), 1_000_000)
  }

  test("supports a self-call threaded through .map over a Vector") {
    assertEquals(deepSumTreeVector(TreeV.Leaf(5)), 5)
    assertEquals(deepSumTreeVector(TreeV.Node(Vector(TreeV.Leaf(1), TreeV.Leaf(2), TreeV.Leaf(3)))), 6)

    val deepChain = (1 to 1_000_000).foldLeft(TreeV.Leaf(1): TreeV)((acc, _) => TreeV.Node(Vector(acc)))
    assertEquals(deepSumTreeVector(deepChain), 1)
  }

  test("supports a self-call threaded through .map over a Set") {
    assertEquals(deepSumTreeSet(TreeS.Leaf(5)), 5)
    assertEquals(deepSumTreeSet(TreeS.Node(Set(TreeS.Leaf(1), TreeS.Leaf(2), TreeS.Leaf(3)))), 6)
  }

  test("supports a self-call threaded through .map over an Option") {
    assertEquals(deepSumOption(Wrapped(1, None)), 1)
    assertEquals(deepSumOption(Wrapped(1, Some(Wrapped(2, Some(Wrapped(3, None)))))), 6)
  }

  test("is stack-safe for deep recursion through .map over an Option") {
    val deepChain = (1 to 1_000_000).foldLeft(Wrapped(0, None))((acc, _) => Wrapped(1, Some(acc)))
    assertEquals(deepSumOption(deepChain), 1_000_000)
  }

  test("supports a self-call threaded through .map over an Either, carrying Left through unchanged") {
    assertEquals(deepSumEither(Chained(1, Left("done"))), 1)
    assertEquals(deepSumEither(Chained(1, Right(Chained(2, Right(Chained(3, Left("done"))))))), 6)
  }

  test("is stack-safe for deep recursion through .map over an Either") {
    val deepChain = (1 to 1_000_000).foldLeft(Chained(0, Left("done")))((acc, _) => Chained(1, Right(acc)))
    assertEquals(deepSumEither(deepChain), 1_000_000)
  }

  test("supports a self-call threaded through .map with deepRecursiveMemoized") {
    assertEquals(deepSumTreeMemoized(Tree.Leaf(5)), 5)
    assertEquals(deepSumTreeMemoized(Tree.Node(List(Tree.Leaf(1), Tree.Leaf(2), Tree.Leaf(3)))), 6)
  }

  test("supports a direct self-call combined with List/Vector/Option/Either .map-threaded self-calls in one branch") {
    val leaf1 = Rich.Leaf(1)
    val leaf2 = Rich.Leaf(2)
    val leaf3 = Rich.Leaf(3)
    val leaf4 = Rich.Leaf(4)
    val leaf5 = Rich.Leaf(5)
    val r = Rich.Node(
      extra = leaf1, // 1
      listKids = List(leaf2, leaf3), // 5
      vecKids = Vector(leaf4), // 4
      optKid = Some(leaf5), // 5
      eitherKid = Right(Rich.Leaf(10)), // 10
    )
    assertEquals(deepSumRich(r), 25)

    val withEmptyOptAndLeft = Rich.Node(leaf1, Nil, Vector.empty, None, Left("x"))
    assertEquals(deepSumRich(withEmptyOptAndLeft), 1)
  }

  test("rejects a self-call threaded through .map over an Array instead of silently mistreating it as a collection") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else Array(n - 1).map(f).sum }""",
    )
    assert(res.contains("cannot safely trampoline"), res)
  }

  test("rejects a self-call threaded through .map over a lazy View or LazyList instead of silently breaking laziness") {
    // the self-call is still element-threaded (`f`, unchanged), isolating the rejection to the
    // collection being lazy rather than to the closure shape (already covered by other tests)
    val viewRes = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else List(n - 1).view.map(f).sum }""",
    )
    assert(viewRes.contains("cannot safely trampoline"), viewRes)

    val lazyListRes = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else LazyList(n - 1).map(f).sum }""",
    )
    assert(lazyListRes.contains("cannot safely trampoline"), lazyListRes)
  }

  test("rejects a self-call threaded through .map whose receiver differs from this instead of silently looping on the original receiver") {
    val res = compileErrors(
      """final case class Box(n: Int, children: List[Box]) { def f(other: Box): Int = deepRecursive { if n == 0 then other.n else children.map(other.f).sum } }""",
    )
    assert(res.contains("receiver"), res)
  }

  test("rejects a self-call threaded through .map over a collection with no TailRecTraversable instance") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else scala.collection.immutable.Queue(n - 1).map(f).sum }""",
    )
    assert(res.contains("cannot safely trampoline"), res)
  }

  test(
    "supports a .map closure that wraps the self-call's result in extra computation, not just threading it through",
  ) {
    assertEquals(deepSumTreePlusOne(Tree.Leaf(5)), 5)
    assertEquals(deepSumTreePlusOne(Tree.Node(List(Tree.Leaf(1), Tree.Leaf(2)))), (1 + 1) + (2 + 1))
  }

  test("supports a .map closure that ignores its own element and calls back into an outer-scope self-call") {
    assertEquals(deepRepeatedCall(0), 1)
    assertEquals(deepRepeatedCall(1), 3)
    assertEquals(deepRepeatedCall(2), 9)
  }

  test("rejects a recursive call nested inside a try instead of silently looping forever") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { try { if n == 0 then 0 else 1 + f(n - 1) } catch case _: Exception => -1 } """,
    )
    assert(res.contains("cannot safely trampoline"), res)
  }

  test("rejects a recursive call nested inside a try within a .map closure instead of silently looping forever") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else List(n - 1).map(x => try f(x) catch case _: Exception => -1).sum }""",
    )
    assert(res.contains("cannot safely trampoline"), res)
  }

  test("rejects a recursive call bound to a lazy val before the tail expression instead of running it eagerly") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else { lazy val prev = f(n - 1); 1 + prev } }""",
    )
    assert(res.contains("lazy val"), res)
  }

  test("rejects a recursive call bound to a var before the tail expression instead of ignoring reassignment") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else { var prev = f(n - 1); 1 + prev } }""",
    )
    assert(res.contains("var"), res)
  }

  test("rejects a recursive call in a non-val statement before the tail expression instead of silently leaving it non-trampolined") {
    val res = compileErrors(
      """def f(n: Int): Int = deepRecursive { if n == 0 then 0 else { f(n - 1); 1 } }""",
    )
    assert(res.contains("final expression"), res)
  }

  test("rejects a recursive call whose receiver differs from this instead of silently looping on the original receiver") {
    val res = compileErrors(
      """final case class Box(n: Int) { def f(other: Box): Int = deepRecursive { if n == 0 then other.n else other.f(Box(n - 1)) } }""",
    )
    assert(res.contains("receiver"), res)
  }

  test("rejects deepRecursive used inside a lambda instead of silently skipping the trampoline") {
    val res = compileErrors(
      """lazy val f: Int => Int = n => deepRecursive { if n == 0 then 0 else 1 + f(n - 1) }""",
    )
    assert(res.contains("body of a named"), res)
  }
