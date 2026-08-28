# commons

halotukozak's small collection of Scala 3 macro and metaprogramming utilities: tuple operations
backed by type-level constraints, typeclass-derivation factories that make `FromExpr`/`ToExpr`
derivation work recursively for generic types, and a macro that rewrites deeply recursive `def`s
into stack-safe trampolines.

Cross-built for the JVM, Scala.js, and Scala Native. Published to Maven Central under `com.halotukozak`.

## Installation

scala-cli:

```scala
//> using dep com.halotukozak::commons::<version>
```

sbt:

```scala
libraryDependencies += "com.halotukozak" %% "commons" % "<version>"
```

## What's inside

### Tuple utilities

- `Tuple.to[T, C]` / `toArrayOf[T]` — convert a tuple into any standard collection (via `Factory`)
  or a plain `Array`, guarded by a `containsOnly` constraint.
- `Tuple.mapAs[T]` — map over a tuple's elements with a function polymorphic in a shared upper bound `T`.
- `Tuple.foreach`, `Tuple.indices`, `Tuple.hasDuplicates` — small ergonomic additions on top of `scala.Tuple`.
- `realCons` — cons an element onto a tuple while preserving its precise singleton type.
- `containsOnly[Tup, T]` — a type-level constraint proving every element of `Tup` conforms to `T`,
  with derivation rules for `Tuple.Map`, covariant functors, `Tail`, `Reverse`, `Concat`, and `Zip`,
  plus implicit conversions from `Head`/`Last`.

### Expr derivation

- `FromExprFactory` / `ToExprFactory` — `derives` on any product or sum type recursively derives
  `FromExpr[T]` / `ToExpr[T]` for each field/case, so generic types no longer need a hand-written
  `given` for their type parameters.
- Built-in instances for `Array`, `Seq`, `List`, `Set`, `Map`, `Option`, `Some`,
  `Either`/`Left`/`Right`, and `Tuple1` through `Tuple22`.
- `QuotedFactoryGivens` bridges any derived factory back into the standard `FromExpr`/`ToExpr`
  so it's picked up automatically wherever those are expected.
- `Expr.ofRefinedTuple` — build an `Expr[Tuple]` from a `List[Expr[?]]` while keeping each
  element's refined type.

```scala
import scala.quoted.*

case class Point(x: Int, y: Int) derives ToExprFactory, FromExprFactory
```

### Deep recursion

- `deepRecursive[T](body: T): T` — rewrites a self-recursive `def` into a trampolined loop at
  compile time, so it runs in constant stack space instead of overflowing on deep input.
- `deepRecursiveMemoized[T](body: T): T` — same rewrite, plus a per-invocation memo table keyed
  on the method's arguments, so overlapping recursive subcalls (e.g. naive Fibonacci) are computed
  only once.

```scala
def deepSum(n: Int): Int = deepRecursive:
  if n == 0 then 0
  else 1 + deepSum(n - 1)

deepSum(1_000_000) // does not stack overflow

def deepFib(n: Int): Int = deepRecursiveMemoized:
  if n < 2 then n
  else deepFib(n - 1) + deepFib(n - 2)
```

The macro must be used directly in the body of a named `def` (not inside a lambda), and it looks
for the recursive call in tail position: the final expression of the body/block, a branch of an
`if`/`match`, or a plain (non-lazy, non-`var`) `val` bound before the final expression. A call
nested under a `try`/`while`/closure, or reached through an unsafe receiver, is rejected at
compile time rather than silently miscompiled.

Recursion is also recognized when threaded through a `.map` call on the recursed-over element —
out of the box for `List`, `Vector`, `Set`, `Option`, and `Either`, and extendable to other
containers via a `given TailRecTraversable[F]`.

```scala
enum Tree:
  case Leaf(n: Int)
  case Node(children: List[Tree])

def deepSumTree(t: Tree): Int = deepRecursive:
  t match
    case Tree.Leaf(n) => n
    case Tree.Node(children) => children.map(deepSumTree).sum

case class Wrapped(n: Int, inner: Option[Wrapped])

def deepSumOption(w: Wrapped): Int = deepRecursive:
  w.n + w.inner.map(deepSumOption).getOrElse(0)
```

## License

MIT
