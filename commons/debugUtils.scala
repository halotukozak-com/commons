package halotukozak
import scala.annotation.publicInBinary
import scala.quoted.*
import scala.util.Try

// $COVERAGE-OFF$
/**
 * Generates a detailed string representation of a symbol during macro expansion.
 *
 * This function produces comprehensive information about a symbol including
 * its owner, flags, names, position, documentation, and structure. It is
 * useful for debugging macro code.
 *
 * @param quotes the Quotes instance
 * @param symbol the symbol to inspect
 * @return a multi-line string with detailed symbol information
 */
private[halotukozak] def symbolInfo(
  using quotes: Quotes,
)(
  symbol: quotes.reflect.Symbol,
)(using quotes.reflect.Printer[quotes.reflect.TypeRepr],
): String =
  s"""
     |${symbol.toString}
     |maybeOwner: ${symbol.maybeOwner.toString}
     |flags: ${symbol.flags.show}
     |privateWithin: ${symbol.privateWithin.map(_.show).toString}
     |protectedWithin: ${symbol.protectedWithin.map(_.show).toString}
     |name: ${symbol.name}
     |fullName: ${symbol.fullName}
     |pos: ${symbol.pos.toString}
     |docstring: ${symbol.docstring.toString}
     |tree: ${Try(symbol.tree.show).getOrElse("no tree")}
     |annotations: ${symbol.annotations.map(_.show).toString}
     |isDefinedInCurrentRun: ${symbol.isDefinedInCurrentRun}
     |isLocalDummy: ${symbol.isLocalDummy}
     |isRefinementClass: ${symbol.isRefinementClass}
     |isAliasType: ${symbol.isAliasType}
     |isAnonymousClass: ${symbol.isAnonymousClass}
     |isAnonymousFunction: ${symbol.isAnonymousFunction}
     |isAbstractType: ${symbol.isAbstractType}
     |isClassConstructor: ${symbol.isClassConstructor}
     |isSuperAccessor: ${symbol.isSuperAccessor}
     |isType: ${symbol.isType}
     |isTerm: ${symbol.isTerm}
     |isPackageDef: ${symbol.isPackageDef}
     |isClassDef: ${symbol.isClassDef}
     |isTypeDef: ${symbol.isTypeDef}
     |isValDef: ${symbol.isValDef}
     |isDefDef: ${symbol.isDefDef}
     |isBind: ${symbol.isBind}
     |isNoSymbol: ${symbol.isNoSymbol}
     |exists: ${symbol.exists}
     |declaredFields: ${symbol.declaredFields.toString}
     |fieldMembers: ${symbol.fieldMembers.toString}
     |declaredMethods: ${symbol.declaredMethods.toString}
     |methodMembers: ${symbol.methodMembers.toString}
     |declaredTypes: ${symbol.declaredTypes.toString}
     |typeMembers: ${symbol.typeMembers.toString}
     |declarations: ${symbol.declarations.toString}
     |paramSymss: ${symbol.paramSymss.toString}
     |allOverriddenSymbols: ${symbol.allOverriddenSymbols.toList.toString}
     |primaryConstructor: ${symbol.primaryConstructor.toString}
     |caseFields: ${symbol.caseFields.toString}
     |isTypeParam: ${symbol.isTypeParam}
     |paramVariance: ${symbol.paramVariance.show}
     |signature: ${symbol.signature.toString}
     |moduleClass: ${symbol.moduleClass.toString}
     |companionClass: ${symbol.companionClass.toString}
     |companionModule: ${symbol.companionModule.toString}
     |children: ${symbol.children.toString}
     |typeRef: ${Try(symbol.typeRef.show).getOrElse("no typeRef")}
     |termRef: ${Try(symbol.termRef.show).getOrElse("no termRef")}
     |""".stripMargin

private[halotukozak] def typeInfo[T: Type](using quotes: Quotes) = typeReprInfo(quotes.reflect.TypeRepr.of[T])

/**
 * Generates a detailed string representation of a type during macro expansion.
 *
 * This function produces comprehensive information about a type including
 * its widened forms, symbols, base classes, and structural properties.
 * It is useful for debugging macro code.
 *
 * @param quotes the Quotes instance
 * @param tpe the type to inspect
 * @return a multi-line string with detailed type information
 */
private[halotukozak] def typeReprInfo(
  using quotes: Quotes,
)(
  tpe: quotes.reflect.TypeRepr,
)(using quotes.reflect.Printer[quotes.reflect.TypeRepr],
): String =
  s"""
     |type: ${tpe.show}
     |raw: ${tpe.toString}
     |widen: ${tpe.widen.show}
     |widenTermRefByName: ${tpe.widenTermRefByName.show}
     |widenByName: ${tpe.widenByName.show}
     |dealias: ${tpe.dealias.show}
     |dealiasKeepOpaques: ${tpe.dealiasKeepOpaques.show}
     |simplified: ${tpe.simplified.show}
     |classSymbol: ${tpe.classSymbol.toString}
     |typeSymbol: ${tpe.typeSymbol.toString}
     |termSymbol: ${tpe.termSymbol.toString}
     |isSingleton: ${tpe.isSingleton}
     |baseClasses: ${tpe.baseClasses.toString}
     |isFunctionType: ${tpe.isFunctionType}
     |isContextFunctionType: ${tpe.isContextFunctionType}
     |isErasedFunctionType: ${tpe.isErasedFunctionType}
     |isDependentFunctionType: ${tpe.isDependentFunctionType}
     |isTupleN: ${tpe.isTupleN}
     |typeArgs: ${tpe.typeArgs.toString}
     |""".stripMargin

private[halotukozak] def compareTypeReprs(
  using quotes: Quotes,
)(
  a: quotes.reflect.TypeRepr,
  b: quotes.reflect.TypeRepr,
)(using pos: Position,
): Nothing =
  compareTypes(using a.asType, b.asType)

private[halotukozak] def compareTypes[T <: AnyKind: Type, U <: AnyKind: Type](using Quotes, Position): Nothing =
  import quotes.reflect.*
  s"""
     |expected:
     |${typeReprInfo(TypeRepr.of[T])}
     |provided:
     |${typeReprInfo(TypeRepr.of[U])}
     |""".stripMargin.dbg

/**
 * Generates a string representation of a tree during macro expansion.
 *
 * This function shows both the structural representation and the short code
 * representation of a tree. It is useful for debugging macro code.
 *
 * @param quotes the Quotes instance
 * @param tree the tree to inspect
 * @return a multi-line string with tree structure and code
 */
private[halotukozak] def treeInfo(using quotes: Quotes)(tree: quotes.reflect.Tree): String =
  import quotes.reflect.*
  s"""
     |Structure ${Printer.TreeStructure.show(tree)}
     |ShortCode ${Printer.TreeShortCode.show(tree)}
     |""".stripMargin

private[halotukozak] def positionInfo(using quotes: Quotes)(pos: quotes.reflect.Position): String =
  s"""
     |start: ${pos.start},
     |end: ${pos.end},
     |startLine: ${pos.startLine},
     |endLine: ${pos.endLine},
     |startColumn: ${pos.startColumn},
     |endColumn: ${pos.endColumn},
     |sourceFile: ${pos.sourceFile.toString},
     |""".stripMargin

inline private[halotukozak] def showAst(inline body: Any) = ${ showAstImpl('{ body }) }

@publicInBinary private[halotukozak] def showAstImpl(body: Expr[Any])(using quotes: Quotes): Expr[Nothing] =
  given Position = Position.NoPosition
  import quotes.reflect.*
  Printer.TreeShortCode.show(body.asTerm.underlyingArgument).dbg

inline private[halotukozak] def showRawAst(inline body: Any) = ${ showRawAstImpl('{ body }) }

@publicInBinary private[halotukozak] def showRawAstImpl(body: Expr[Any])(using quotes: Quotes): Expr[Nothing] =
  given Position = Position.NoPosition
  import quotes.reflect.*
  Printer.TreeStructure.show(body.asTerm.underlyingArgument).dbg

extension (s: String)
  private[halotukozak] def dbg(using position: Position)(using quotes: Quotes): Nothing =
    import quotes.reflect.*
    report.errorAndAbort(s"$s ${position.toString}")
  private[halotukozak] def info(using position: Position)(using quotes: Quotes): String =
    import quotes.reflect.*
    report.info(s"$s ${position.toString}")
    s

inline private[halotukozak] def showTypeRepr[T] = ${ showTypeReprImpl[T] }

@publicInBinary private[halotukozak] def showTypeReprImpl[T: Type](using Quotes): Expr[Nothing] =
  given Position = Position.NoPosition
  import quotes.reflect.*
  typeReprInfo(TypeRepr.of[T]).dbg

private[halotukozak] def wontHappen(using Quotes, Position) =
  s"This code should never be executed".dbg
// $COVERAGE-ON$

private[halotukozak] case class Position(
  startLine: Int,
  startColumn: Int,
  sourceFile: String,
):
  override def toString: String = s"at line $startLine, column $startColumn in $sourceFile"

@publicInBinary private[halotukozak] object Position:
  private[halotukozak] object NoPosition extends Position(-1, -1, "<no source file>"):
    override def toString: String = "<no position>"
  inline private[halotukozak] given Position = ${ impl }
  @publicInBinary private[halotukozak] def impl(using quotes: Quotes): Expr[Position] =
    val pos = quotes.reflect.Position.ofMacroExpansion
    '{
      Position(
        startLine = ${ Expr(pos.startLine) },
        startColumn = ${ Expr(pos.startColumn) },
        sourceFile = ${ Expr(pos.sourceFile.name) },
      )
    }
