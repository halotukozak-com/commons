package scala
package quoted

import scala.annotation.unused

// Mirrors the priority ordering scala/scala3#26541 gives its two bridging givens in
// FromExpr's/ToExpr's own companions (only the standalone factories were copied into
// this project, so the bridges live here instead, in a low-priority base trait).
// ValueOfToExpr sits in the subtrait, so it outranks the fully generic factory
// bridge for singleton/module types with no `derives` clause (e.g. a plain
// `object Marker`) via the usual "subclass beats inherited" priority rule; anything
// with a concrete `derives FromExprFactory`/`ToExprFactory` instance is picked
// automatically by Scala's specificity check regardless of this ordering.
trait LowPriorityQuotedFactoryGivens:
  given fromExprFactoryBridge: [T: Type] => (f: FromExprFactory[T]) => FromExpr[T] = f.apply()
  given toExprFactoryBridge: [T: Type] => (f: ToExprFactory[T]) => ToExpr[T] = f.apply()

trait QuotedFactoryGivens extends LowPriorityQuotedFactoryGivens:
  given ValueOfToExpr: [T: {Type}] => (@unused ev: ValueOf[T]) => ToExpr[T]:
    def apply(x: T)(using Quotes): Expr[T] = '{ valueOf[T] }

object QuotedFactoryGivens extends QuotedFactoryGivens
