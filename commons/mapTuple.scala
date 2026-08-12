package commons

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

trait TupleMapper[U]:
  transparent inline def apply[T <: U](t: T): Any

object TupleMapper:
  inline def apply[U](f: [e <: U] => e => Any): TupleMapper[U] =
    compiletime.error("TupleMapper.apply is not meant to be called directly. Use the `mapTuple` macro instead.")

private def mapTupleImplCore[U: Type, Tup <: Tuple: Type](
  f: Expr[TupleMapper[U]],
)(
  elementAt: [E <: U: Type] => Int => Expr[E],
)(using quotes: Quotes,
): Expr[Tuple] =
  import quotes.reflect.*

  val extractSymbol: PartialFunction[Statement, (Symbol, Symbol, Term)] = {
    case DefDef(
          _,
          List(TypeParamClause(List(tparam)), TermParamClause(List(paramDef: ValDef))),
          _,
          Some(rhs),
        ) =>
      (tparam.symbol, paramDef.symbol, rhs)
  }

  def fromLambda = Some(f)
    .collect:
      case '{ TupleMapper[U]($lambda) } => lambda.asTerm
    .collect:
      case Block(List(extractSymbol(result)), _) => result

  def fromAnonymousClass = Some(f.asTerm.underlying)
    .collect:
      case Block(List(ClassDef(_, _, _, _, body)), _) => body
    .flatMap:
      val TupleMapperApplySym = TypeRepr.of[TupleMapper].typeSymbol.methodMember("apply").head
      _.collectFirst:
        case defdef @ extractSymbol(result) if defdef.symbol.allOverriddenSymbols contains TupleMapperApplySym =>
          result

  val (tparamSymbol, paramSymbol, rhs) = fromLambda
    .orElse(fromAnonymousClass)
    .getOrElse:
      report.errorAndAbort(
        "Not supported TupleMapper implementation. Expected a lambda or an anonymous class with an `apply` method.",
      )

  def callRhs[E <: U: Type](index: Int): Expr[?] = {
    new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        def elseBranch =
          val term = tree match
            case block: Block => block.changeOwner(owner)
            case other => other
          super.transformTerm(term)(owner)

        if tree.symbol == paramSymbol then elementAt[E](index).asTerm
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
  inline f: TupleMapper[U],
)(using tup.type containsOnly U,
): Tuple =
  ${ mapTupleImpl[U, tup.type]('tup, 'f) }

def mapTupleImpl[U: Type, Tup <: Tuple: Type](
  tup: Expr[Tup],
  f: Expr[TupleMapper[U]],
)(using Quotes,
): Expr[Tuple] =
  mapTupleImplCore[U, Tup](f)([E: Type] => (index: Int) => '{ $tup(${ Expr(index) }).asInstanceOf[E] })

transparent inline def mapTuple[Tup <: Tuple, U](
  inline f: TupleMapper[U],
)(using Tup containsOnly U,
): Tuple =
  ${ mapTupleImpl[U, Tup]('f) }

def mapTupleImpl[U: Type, Tup <: Tuple: Type](f: Expr[TupleMapper[U]])(using Quotes): Expr[Tuple] =
  mapTupleImplCore[U, Tup](f)([E: Type] => (_: Int) => '{ compiletime.erasedValue[E] })
