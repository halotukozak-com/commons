package halotukozak
package commons
import scala.annotation.tailrec
import scala.quoted.*
import scala.util.control.TailCalls.{done, tailcall, TailRec}

inline def deepRecursive[T](inline body: T): T = ${ deepRecursiveImpl[T]('body) }
def deepRecursiveImpl[T](body: Expr[T])(using Quotes, Type[T]): Expr[T] =
  import quotes.reflect.*

  val methSymbol = Symbol.spliceOwner.owner
  if methSymbol.flags.is(Flags.Synthetic) then
    report.errorAndAbort(
      "deepRecursive: must be used directly in the body of a named `def`, not inside a lambda " +
        "(e.g. a `val`/`lazy val` holding a function value) - recursive calls there reference " +
        "the val, not this closure, so they can't be trampolined",
    )
  val termParams = methSymbol.paramSymss.flatten.filter(_.isTerm)

  val loopMethod = Symbol.newMethod(
    methSymbol,
    Symbol.freshName("loop"),
    MethodType(termParams.map(_.name))(
      _ => termParams.map(_.termRef.widen),
      _ => TypeRepr.of[TailRec].appliedTo(TypeRepr.of[T]),
    ),
  )

  object selfCallCollector extends TreeAccumulator[List[Apply]]:
    def foldTree(acc: List[Apply], tree: Tree)(owner: Symbol): List[Apply] = tree match
      case app @ Apply(fun, _) if fun.symbol == methSymbol => app :: acc
      case _: If | _: Match | _: Try | _: While | _: Closure | _: DefDef =>
        foldOverTree(Nil, tree)(owner) match
          case Nil => acc
          case unsafe =>
            report.errorAndAbort(
              "deepRecursive: recursive call is nested under a condition, loop, try, or closure " +
                "that this macro cannot safely trampoline (it would run unconditionally and only " +
                "once instead of following the original control flow)",
              unsafe.head.pos,
            )
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

    def buildChain(remaining: List[Apply], bound: Vector[(Term, Expr[T])]): Expr[TailRec[T]] = remaining match
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
      Block(stats, transform(expr))
    case Typed(expr, _) =>
      transform(expr)
    case Inlined(call, bindings, expr) =>
      Inlined(call, bindings, transform(expr))
    case _ =>
      wrapLeaf(tree)

  val paramSubstitution = termParams.iterator.zip(loopMethod.paramSymss.iterator.flatten).toMap

  object renameParams extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term = t match
      case ident: Ident if paramSubstitution.contains(ident.symbol) =>
        Ref(paramSubstitution(ident.symbol))
      case _ => super.transformTerm(t)(owner)

  val renamedBody = renameParams.transformTerm(body.asTerm)(loopMethod)
  val loopBody = transform(renamedBody).changeOwner(loopMethod)
  val loopDefDef = DefDef(loopMethod, _ => Some(loopBody))
  val loopCall = Ref(loopMethod).appliedToArgs(termParams.map(Ref.apply)).asExprOf[TailRec[T]]

  Block(List(loopDefDef), '{ $loopCall.result }.asTerm).asExprOf[T]
