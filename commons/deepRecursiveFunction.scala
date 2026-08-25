package halotukozak
package commons
import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*
import scala.util.control.TailCalls.{done, tailcall, TailRec}

inline def deepRecursive[T](inline body: T): T = ${ deepRecursiveImpl[T]('body, false) }
inline def deepRecursiveMemoized[T](inline body: T): T = ${ deepRecursiveImpl[T]('body, true) }
def deepRecursiveImpl[T](body: Expr[T], memoized: Boolean)(using Quotes, Type[T]): Expr[T] =
  import quotes.reflect.*

  val methSymbol = Symbol.spliceOwner.owner
  if methSymbol.flags.is(Flags.Synthetic) then
    report.errorAndAbort(
      "deepRecursive: must be used directly in the body of a named `def`, not inside a lambda " +
        "(e.g. a `val`/`lazy val` holding a function value) - recursive calls there reference " +
        "the val, not this closure, so they can't be trampolined",
    )
  val termParams = methSymbol.paramSymss.flatten.filter(_.isTerm)

  val memoizedResultsSymbol = Option.when(memoized):
    Symbol.newVal(
      parent = methSymbol,
      name = Symbol.freshName("memoizedResults"),
      tpe = TypeRepr.of[mutable.Map[Any, TailRec[T]]],
      flags = Flags.EmptyFlags,
      privateWithin = Symbol.noSymbol,
    )

  val loopMethod = Symbol.newMethod(
    parent = methSymbol,
    name = Symbol.freshName("loop"),
    tpe = MethodType(termParams.map(_.name))(
      _ => termParams.map(_.termRef.widen),
      _ => TypeRepr.of[TailRec[T]],
    ),
  )

  object selfCallCollector extends TreeAccumulator[List[Apply]]:
    @tailrec def unsafeReceiver(tree: Term): Option[Term] = tree match
      case Apply(fun, _) => unsafeReceiver(fun)
      case Select(This(_), _) => None
      case Select(qual, _) => Some(qual)
      case _ => None

    def foldTree(acc: List[Apply], tree: Tree)(owner: Symbol): List[Apply] = tree match
      case app @ Apply(fun, _) if fun.symbol == methSymbol =>
        unsafeReceiver(fun) match
          case Some(receiver) =>
            report.errorAndAbort(
              "deepRecursive: recursive call's receiver is not `this` (e.g. `y.foo(...)` where `y` " +
                "is a different value than the enclosing instance) - only the explicit arguments are " +
                "threaded through the trampoline, so the receiver would be silently dropped and the " +
                "call would keep recursing against the original receiver instead of advancing to `y`; " +
                "turn this into an `extension` method so the receiver becomes an explicit parameter",
              receiver.pos,
            )
          case None => app :: acc
      case _: If | _: Match | _: Try | _: While | _: Closure | _: DefDef =>
        foldOverTree(Nil, tree)(owner) match
          case Nil => acc
          case unsafe =>
            unsafe.foreach: call =>
              report.error(
                "deepRecursive: recursive call is nested under a condition, loop, try, or closure " +
                  "that this macro cannot safely trampoline (it would run unconditionally and only " +
                  "once instead of following the original control flow)",
                call.pos,
              )
            Nil
      case _ => foldOverTree(acc, tree)(owner)

  @tailrec def flattenArgs(tree: Term, acc: List[Term] = Nil): List[Term] = tree match
    case Apply(fun, args) => flattenArgs(fun, args ::: acc)
    case _ => acc

  def replaceSubtrees(tree: Term, mapping: Seq[(find: Term, replace: Expr[T])]): Expr[T] =
    object replacer extends TreeMap:
      override def transformTerm(t: Term)(owner: Symbol): Term =
        mapping.find(_.find eq t).map(_.replace.asTerm).getOrElse(super.transformTerm(t)(owner))

    replacer.transformTerm(tree)(Symbol.spliceOwner).asExprOf[T]

  def wrapLeaf(tree: Term): Term =
    val calls = selfCallCollector.foldTree(Nil, tree)(Symbol.spliceOwner).reverse

    def buildChain(remaining: List[Apply], bound: Vector[(find: Term, replace: Expr[T])]): Expr[TailRec[T]] =
      remaining match
        case Nil => '{ done[T](${ replaceSubtrees(tree, bound) }) }
        case (call @ Apply(_, _)) :: rest =>
          '{
            tailcall(${ Ref(loopMethod).appliedToArgs(flattenArgs(call)).asExprOf[TailRec[T]] }).flatMap { (x: T) =>
              ${ buildChain(rest, bound :+ (call, '{ x })) }
            }
          }

    buildChain(calls, Vector.empty).asTerm

  def transform(tree: Term): Term = tree match
    case If(cond, thenp, elsep) =>
      If(cond, transform(thenp), transform(elsep))
    case Match(scrutinee, cases) =>
      Match(scrutinee, cases.map(c => CaseDef(c.pattern, c.guard, transform(c.rhs))))
    case Block(stats, expr) =>
      stats.foreach: stat =>
        selfCallCollector.foldTree(Nil, stat)(Symbol.spliceOwner) match
          case Nil => ()
          case unsafe =>
            unsafe.foreach: call =>
              report.error(
                "deepRecursive: recursive call happens in a statement before the block's final " +
                  "expression (e.g. bound to a `val`) - this macro only trampolines calls it finds " +
                  "in the final expression, so a call sequenced earlier would run as an ordinary, " +
                  "non-tail, stack-consuming call instead of being trampolined",
                call.pos,
              )
            Nil
      Block(stats, transform(expr))
    case Typed(expr, _) =>
      transform(expr)
    case Inlined(call, bindings, expr) =>
      Inlined(call, bindings, transform(expr))
    case _ =>
      wrapLeaf(tree)

  val memoizedResultsValDef = memoizedResultsSymbol.map(ValDef(_, Some('{ mutable.Map.empty[Any, TailRec[T]] }.asTerm)))

  val loopDefDef = DefDef(
    loopMethod,
    args =>
      object renameParams extends TreeMap:
        private val paramSubstitution = termParams.iterator.zip(args.flatten).toMap

        override def transformTerm(t: Term)(owner: Symbol): Term = t match
          case ident: Ident if paramSubstitution.contains(ident.symbol) =>
            paramSubstitution(ident.symbol).asInstanceOf[Term]
          case _ => super.transformTerm(t)(owner)

      val renamedBody = renameParams.transformTerm(body.asTerm)(loopMethod)
      val loopBody = transform(renamedBody).changeOwner(loopMethod)

      memoizedResultsValDef match
        case Some(valDef) =>
          val map = Ref(valDef.symbol).asExprOf[mutable.Map[Any, TailRec[T]]]
          val arguments: Expr[Any] = args.flatten match
            case List(single) => single.asExpr
            case multiple => Expr.ofTupleFromSeq(multiple.map(_.asExpr))

          Some('{ $map.getOrElseUpdate($arguments, ${ loopBody.asExprOf[TailRec[T]] }) }.asTerm.changeOwner(loopMethod))
        case _ =>
          Some(loopBody),
  )
  val loopCall = Ref(loopMethod).appliedToArgs(termParams.map(Ref.apply)).asExprOf[TailRec[T]]

  Block(memoizedResultsValDef.toList :+ loopDefDef, '{ $loopCall.result }.asTerm).asExprOf[T]
