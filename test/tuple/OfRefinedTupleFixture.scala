package halotukozak.commons

import scala.quoted.*

object OfRefinedTupleFixture:
  inline def triple: (1, "a", true) = ${ tripleImpl }
  inline def empty: EmptyTuple = ${ emptyImpl }

  // `.asExprOf` is the regression guard: it fails at expansion if `ofRefinedTuple`
  // widened the constant element types to `(Int, String, Boolean)` instead of
  // keeping `(1, "a", true)`.
  def tripleImpl(using Quotes): Expr[(1, "a", true)] =
    Expr.ofRefinedTuple(List(Expr(1), Expr("a"), Expr(true))).asExprOf[(1, "a", true)]

  def emptyImpl(using Quotes): Expr[EmptyTuple] =
    Expr.ofRefinedTuple(Nil).asExprOf[EmptyTuple]
