package commons

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

sealed class TupleMapScope
def deferredSummon[T](using TupleMapScope): T = ???

private def mapTupleImplCore[U: Type, Tup <: Tuple: Type](
  f: Expr[TupleMapScope ?=> [e <: U] => e => Any],
)(
  elementAt: [E <: U: Type] => Int => Expr[E],
)(using quotes: Quotes,
): Expr[Tuple] =
  import quotes.reflect.*

  f.asTerm.underlying match
    case Lambda(
          List(_),
          Block(
            List(
              DefDef(
                _,
                List(TypeParamClause(List(tparam)), TermParamClause(List(paramDef @ ValDef(_, _, _)))),
                _,
                Some(rhs),
              ),
            ),
            _,
          ),
        ) =>
      val tparamSymbol = tparam.symbol
      val paramSymbol = paramDef.symbol

      def callRhs[E <: U: Type](index: Int): Expr[?] = {
        new TreeMap:
          override def transformTerm(tree: Term)(owner: Symbol): Term =
            def elseBranch =
              val term = tree match
                case block: Block => block.changeOwner(owner)
                case other => other
              super.transformTerm(term)(owner)

            if tree.symbol == paramSymbol then elementAt[E](index).asTerm
            else if tree.isExpr then
              tree.asExpr match
                case '{ deferredSummon[t](using $_) } =>
                  transformTypeTree(TypeTree.of[t])(owner).tpe.asType match
                    case '[tt] =>
                      '{ compiletime.summonInline[tt] }.asTerm
                case _ => elseBranch
            else elseBranch

          override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
            val substituted = tree.tpe.substituteTypes(List(tparamSymbol), List(TypeRepr.of[E]))
            if substituted != tree.tpe then TypeTree.of(using substituted.asType)
            else super.transformTypeTree(tree)(owner)
      }.transformTerm(rhs)(Symbol.spliceOwner).asExpr

      @tailrec def loop[tuple <: Tuple: Type](acc: Vector[Expr[Any]], index: Int): Expr[Tuple] = Type.of[tuple] match
        case '[EmptyTuple] =>
          Expr.ofRefinedTuple(acc.toList)
        case '[type h <: U; h *: tail] =>
          val headExpr = callRhs[h](index)
          loop[tail](acc :+ headExpr, index + 1)

      loop[Tup](Vector.empty, 0)

transparent inline def mapTuple(
  tup: Tuple,
)[U](
  inline f: TupleMapScope ?=> [e <: U] => e => Any,
)(using tup.type containsOnly U,
): Tuple =
  ${ mapTupleImpl[U, tup.type]('tup, 'f) }

def mapTupleImpl[U: Type, Tup <: Tuple: Type](
  tup: Expr[Tup],
  f: Expr[TupleMapScope ?=> [e <: U] => e => Any],
)(using Quotes,
): Expr[Tuple] =
  mapTupleImplCore[U, Tup](f)([E: Type] => (index: Int) => '{ $tup(${ Expr(index) }).asInstanceOf[E] })

transparent inline def mapTuple[Tup <: Tuple, U](
  inline f: TupleMapScope ?=> [e <: U] => e => Any,
)(using Tup containsOnly U,
): Tuple =
  ${ mapTupleImpl[U, Tup]('f) }

def mapTupleImpl[U: Type, Tup <: Tuple: Type](f: Expr[TupleMapScope ?=> [e <: U] => e => Any])(using Quotes)
  : Expr[Tuple] =
  mapTupleImplCore[U, Tup](f)([E: Type] => (_: Int) => '{ compiletime.erasedValue[E] })
