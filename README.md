# commons

halotukozak's small collection of Scala 3 macro and metaprogramming utilities: tuple operations
backed by type-level constraints, and typeclass-derivation factories that make `FromExpr`/`ToExpr`
derivation work recursively for generic types.

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

## License

MIT
