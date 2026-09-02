package halotukozak.commons

import scala.quoted.*

extension (companion: Expr.type)
  /**
   * Build an `Expr[Tuple]` from a `List[Expr[?]]`, keeping the precise
   * `h0 *: h1 *: … *: EmptyTuple` element type.
   *
   * The emitted term is a single flat `Tuple.fromArray(Array(…)).asInstanceOf[…]`,
   * not a nested `h0 *: (h1 *: (…))`. A nested chain re-ascribes the whole remaining
   * tuple type at every cons, so it expands to O(n²) tree size wherever it lands at
   * an `inline` use site — measurably the bulk of the inlining phase in
   * derivation-heavy downstreams (mirror `elems`, per-element annotation flags, …).
   * The flat form is O(n).
   */
  def ofRefinedTuple(exprs: List[Expr[?]])(using Quotes): Expr[Tuple] =
    import quotes.reflect.*
    exprs match
      case Nil => '{ EmptyTuple }
      case _ =>
        val tpe = exprs.foldRight(TypeRepr.of[EmptyTuple]): (e, acc) =>
          TypeRepr.of[*:].appliedTo(List(e.asTerm.tpe.widenTermRefByName, acc))
        tpe.asType match
          case '[type t <: Tuple; t] => '{ Tuple.fromArray(Array(${ Varargs[Any](exprs) }*)).asInstanceOf[t] }
