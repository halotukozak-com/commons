package commons

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

private def mapTupleImplCore[U: Type, Tup <: Tuple: Type](
  f: Expr[[e <: U] => e => Any],
)(
  elementAt: [E: Type] => Int => Expr[Any],
)(using quotes: Quotes,
): Expr[Tuple] =
  import quotes.reflect.*

  f.asTerm.underlying match
    case Block(
          List(
            DefDef(
              _,
              List(TypeParamClause(List(tparam)), TermParamClause(List(paramDef @ ValDef(_, _, _)))),
              _,
              Some(rhs),
            ),
          ),
          _,
        ) =>
      val tparamSymbol = tparam.symbol
      val paramSymbol = paramDef.symbol

      def callRhs[E: Type](index: Int): Expr[?] = {
        new TreeMap:
          override def transformTerm(tree: Term)(owner: Symbol): Term =
            if tree.symbol == paramSymbol then elementAt[E](index).asTerm
            else
              val term = tree match
                case block: Block => block.changeOwner(owner)
                case other => other
              super.transformTerm(term)(owner)

          override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
            val substituted = tree.tpe.substituteTypes(List(tparamSymbol), List(TypeRepr.of[E]))
            if substituted != tree.tpe then TypeTree.of(using substituted.asType)
            else super.transformTypeTree(tree)(owner)
      }.transformTerm(rhs)(Symbol.spliceOwner).asExpr

      @tailrec def loop[tuple <: Tuple: Type](acc: Vector[Expr[Any]], index: Int): Expr[Tuple] = Type.of[tuple] match
        case '[EmptyTuple] =>
          Expr.ofRefinedTuple(acc.toList)
        case '[h *: tail] =>
          val headExpr = callRhs[h](index)
          loop[tail](acc :+ headExpr, index + 1)

      loop[Tup](Vector.empty, 0)

transparent inline def mapTuple(tup: Tuple)[U](inline f: [e <: U] => e => Any)(using tup.type containsOnly U): Tuple =
  ${ mapTupleImpl[U, tup.type]('tup, 'f) }

def mapTupleImpl[U: Type, Tup <: Tuple: Type](tup: Expr[Tup], f: Expr[[e <: U] => e => Any])(using Quotes)
  : Expr[Tuple] =
  mapTupleImplCore[U, Tup](f)([E: Type] => (index: Int) => '{ $tup(${ Expr(index) }) })

transparent inline def mapTuple[Tup <: Tuple, U](inline f: [e <: U] => e => Any)(using Tup containsOnly U): Tuple =
  ${ mapTupleImpl[U, Tup]('f) }

def mapTupleImpl[U: Type, Tup <: Tuple: Type](f: Expr[[e <: U] => e => Any])(using Quotes): Expr[Tuple] =
  mapTupleImplCore[U, Tup](f)([E: Type] => (_: Int) => '{ compiletime.erasedValue[E] })
