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

  class SubstituteIdents(substitution: Map[Symbol, Term]) extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term = t match
      case ident: Ident if substitution.contains(ident.symbol) => substitution(ident.symbol)
      case _ => super.transformTerm(t)(owner)

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

  val MapSelect: PartialFunction[Term, Term] =
    case Select(qual, "map") => qual
    case TypeApply(Select(qual, "map"), _) => qual

  object tailRecTraversablesCache:
    private val underlying = mutable.Map.empty[Type[?], Option[Expr[TailRecTraversable[?]]]]

    private def tailRecTraversableApplied(tpe: TypeRepr): Option[Type[TailRecTraversable[?]]] =
      tpe match
        case AppliedType(tycon, args) if args.nonEmpty =>
          val fixedArgs = args.init
          val ctor = if fixedArgs.isEmpty then tycon
          else
            TypeLambda(
              List("X"),
              _ => List(TypeBounds.empty),
              tl => AppliedType(tycon, fixedArgs :+ tl.param(0)),
            )
          ctor.asType match
            case '[type f[_]; f] =>
              Some(Type.of[TailRecTraversable[f]].asInstanceOf[Type[TailRecTraversable[?]]])
        case _ => None

    def get(tpe: TypeRepr): Option[Expr[TailRecTraversable[?]]] =
      tailRecTraversableApplied(tpe).flatMap: tc =>
        underlying.getOrElseUpdate(tc, Expr.summon(using tc))

    def exists(tpe: TypeRepr): Boolean = get(tpe).isDefined

  object stripTransparent:
    @tailrec def unapply(tree: Term): Some[Term] = tree.underlyingArgument match
      case Typed(e, _) => stripTransparent.unapply(e)
      case other => Some(other)

  object selfCallCollector extends TreeAccumulator[List[Apply]]:
    @tailrec def unsafeReceiver(tree: Term): Option[Term] = tree match
      case Apply(fun, _) => unsafeReceiver(fun)
      case Select(This(_), _) => None
      case Select(qual, _) => Some(qual)
      case _ => None

    object mentionsSelfCall extends TreeAccumulator[Boolean]:
      def foldTree(acc: Boolean, tree: Tree)(owner: Symbol): Boolean = acc ||
        (tree match
          case Apply(fun, _) if fun.symbol == methSymbol => true
          case _ => foldOverTree(acc, tree)(owner))

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
      case app @ Apply(MapSelect(qual), List(fnArg))
          if tailRecTraversablesCache.exists(qual.tpe.widen) &&
            mentionsSelfCall.foldTree(false, fnArg)(Symbol.spliceOwner) =>
        app :: acc
      case _: If | _: Match | _: Try | _: While | _: Closure | _: DefDef =>
        foldOverTree(Nil, tree)(owner) match
          case Nil => acc
          case unsafe =>
            unsafe.foreach: call =>
              report.error(
                """|recursive call is nested under a condition, loop, try, or closure
                   |that this macro cannot safely trampoline (it would run unconditionally and only
                   |once instead of following the original control flow)
                   |""".stripIndent(),
                call.pos,
              )
            Nil
      case _ => foldOverTree(acc, tree)(owner)

  @tailrec def flattenArgs(tree: Term, acc: List[Term] = Nil): List[Term] = tree match
    case Apply(fun, args) => flattenArgs(fun, args ::: acc)
    case _ => acc

  def replaceSubtrees(tree: Term, mapping: Seq[(find: Term, replace: Expr[Any])]): Term =
    object substituter extends TreeMap:
      override def transformTerm(t: Term)(owner: Symbol): Term =
        mapping.find(_.find eq t).map(_.replace.asTerm).getOrElse(super.transformTerm(t)(owner))

    substituter.transformTerm(tree)(Symbol.spliceOwner)

  def substituteIdent(tree: Term, symbol: Symbol, replacement: Term): Term =
    object substituter extends TreeMap:
      override def transformTerm(t: Term)(owner: Symbol): Term = t match
        case ident: Ident if ident.symbol == symbol => replacement
        case _ => super.transformTerm(t)(owner)

    substituter.transformTerm(tree)(Symbol.spliceOwner)

  def containsSelfCall(tree: Tree): Boolean =
    object detector extends TreeAccumulator[Boolean]:
      def foldTree(acc: Boolean, tree: Tree)(owner: Symbol): Boolean =
        if acc then true
        else
          tree match
            case Apply(fun, _) if fun.symbol == methSymbol => true
            case _ => foldOverTree(acc, tree)(owner)

    detector.foldTree(false, tree)(Symbol.spliceOwner)

  def wrapLeaf(tree: Term, cont: Term => Term): Term =
    val calls = selfCallCollector.foldTree(Nil, tree)(Symbol.spliceOwner).reverse

    def buildChain(remaining: List[Apply], bound: Vector[(find: Term, replace: Expr[Any])]): Expr[TailRec[T]] =
      remaining match
        case Nil => cont(replaceSubtrees(tree, bound)).asExprOf[TailRec[T]]
        case (call @ Apply(fun, _)) :: rest if fun.symbol == methSymbol =>
          '{
            tailcall(${ Ref(loopMethod).appliedToArgs(flattenArgs(call)).asExprOf[TailRec[T]] }).flatMap { (x: T) =>
              ${ buildChain(rest, bound :+ (call, '{ x })) }
            }
          }
        case (call @ Apply(MapSelect(qual), List(stripTransparent(Lambda(List(param), rhs))))) :: rest =>
          val elemTpe = qual.tpe.widen match
            case AppliedType(_, args) if args.nonEmpty => args.last
            case _ => report.errorAndAbort("could not destructure the traversed container's type", call.pos)

          elemTpe.asType match
            case '[elem] =>
              val stepExpr: Expr[elem => TailRec[T]] =
                '{ (e: elem) =>
                  ${
                    val substituted =
                      SubstituteIdents(Map(param.symbol -> '{ e }.asTerm)).transformTerm(rhs)(Symbol.spliceOwner)
                    transform(substituted, t => '{ done[T](${ t.asExprOf[T] }) }.asTerm).asExprOf[TailRec[T]]
                  }
                }
              tailRecTraversablesCache.get(qual.tpe.widen) match
                case Some('{ $evExpr: TailRecTraversable[f] }) =>
                  '{
                    tailcall($evExpr.traverse[elem, T](${ qual.asExprOf[f[elem]] })($stepExpr)).flatMap { (xs: f[T]) =>
                      ${ buildChain(rest, bound :+ (call, '{ xs })) }
                    }
                  }
                case _ =>
                  report.errorAndAbort(
                    "recognized a .map traversal but found no TailRecTraversable instance for it",
                    call.pos,
                  )

        case (call @ Apply(MapSelect(_), List(_))) :: _ =>
          report.errorAndAbort("could not destructure the .map closure", call.pos)
        case (call @ Apply(_, _)) :: _ =>
          report.errorAndAbort("unrecognized self-call shape", call.pos)

    buildChain(calls, Vector.empty).asTerm

  def transform(tree: Term, cont: Term => Term): Term = tree match
    case If(cond, thenp, elsep) =>
      If(cond, transform(thenp, cont), transform(elsep, cont))
    case Match(scrutinee, cases) =>
      Match(scrutinee, cases.map(c => CaseDef(c.pattern, c.guard, transform(c.rhs, cont))))
    case Block(stats, expr) =>
      transformBlock(stats, expr, cont)
    case Typed(expr, _) =>
      transform(expr, cont)
    case Inlined(call, bindings, expr) =>
      Inlined(call, bindings, transform(expr, cont))
    case _ =>
      wrapLeaf(tree, cont)

  def transformBlock(stats: List[Statement], expr: Term, cont: Term => Term): Term = stats match
    case Nil =>
      transform(expr, cont)
    case (valDef @ ValDef(_, _, Some(rhs))) :: rest
        if containsSelfCall(rhs) && !valDef.symbol.flags.is(Flags.Mutable) && !valDef.symbol.flags.is(Flags.Lazy) =>
      transform(
        rhs,
        boundValue =>
          substituteIdent(Block(rest, expr), valDef.symbol, boundValue).match
            case Block(newStats, newExpr) => transformBlock(newStats, newExpr, cont)
            case newExpr => transformBlock(Nil, newExpr, cont),
      )
    case (valDef @ ValDef(_, _, Some(rhs))) :: rest if containsSelfCall(rhs) =>
      selfCallCollector
        .foldTree(Nil, rhs)(Symbol.spliceOwner)
        .foreach: call =>
          report.error(
            "deepRecursive: recursive call is bound to a `var` or `lazy val` before the block's final " +
              "expression - only a plain, non-lazy `val` can be substituted safely, since a `var` may be " +
              "reassigned and a `lazy val`'s initializer must stay deferred instead of running eagerly here",
            call.pos,
          )
      Block(List(valDef), transformBlock(rest, expr, cont))
    case other :: rest =>
      selfCallCollector.foldTree(Nil, other)(Symbol.spliceOwner) match
        case Nil => ()
        case unsafe =>
          unsafe.foreach: call =>
            report.error(
              "deepRecursive: recursive call happens in a statement before the block's final " +
                "expression - this macro only trampolines calls it finds in the final expression or in " +
                "a plain `val`'s initializer, so a call sequenced earlier in any other kind of statement " +
                "would run as an ordinary, non-tail, stack-consuming call instead of being trampolined",
              call.pos,
            )
      Block(List(other), transformBlock(rest, expr, cont))

  val memoizedResultsValDef = memoizedResultsSymbol.map(ValDef(_, Some('{ mutable.Map.empty[Any, TailRec[T]] }.asTerm)))

  val loopDefDef = DefDef(
    loopMethod,
    args =>
      val paramSubstitution = termParams.iterator.zip(args.flatten.map(_.asInstanceOf[Term])).toMap
      val renamedBody = SubstituteIdents(paramSubstitution).transformTerm(body.asTerm)(loopMethod)
      val loopBody = transform(renamedBody, t => '{ done[T](${ t.asExprOf[T] }) }.asTerm).changeOwner(loopMethod)

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
