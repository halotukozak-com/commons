package scala
package quoted

import scala.annotation.unused

// Mirrors the priority ordering scala/scala3#26541 gives its two bridging givens in
// FromExpr's/ToExpr's own companions (only the standalone factories were copied into
// this project, so the bridges live here instead, in low-priority base traits).
// `toExprFactoryBridge` sits in the subtrait, so it outranks `ValueOfToExpr`: a
// concrete `derives ToExprFactory` instance always wins over the blind `ValueOf`-based
// fallback, which is only meant to cover a genuine non-derived singleton (e.g. a plain
// `object Marker`). Getting this backwards silently breaks round-tripping for any
// `derives ToExprFactory, FromExprFactory` singleton: `ValueOfToExpr` would still build
// the `Expr` (as `valueOf[T]`), but `FromExprFactory`'s derived `unapply` only
// recognizes a bare reference to the singleton's symbol, not a `valueOf[T]` call.
trait LowestPriorityQuotedFactoryGivens:
  given ValueOfToExpr: [T: {Type}] => (@unused ev: ValueOf[T]) => ToExpr[T]:
    def apply(x: T)(using Quotes): Expr[T] = '{ valueOf[T] }

trait LowPriorityQuotedFactoryGivens extends LowestPriorityQuotedFactoryGivens:
  given fromExprFactoryBridge: [T: Type] => (f: FromExprFactory[T]) => FromExpr[T] = f.apply()
  given toExprFactoryBridge: [T: Type] => (f: ToExprFactory[T]) => ToExpr[T] = f.apply()

trait QuotedFactoryGivens extends LowPriorityQuotedFactoryGivens

object QuotedFactoryGivens extends QuotedFactoryGivens
