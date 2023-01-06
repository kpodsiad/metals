package scala.meta.internal.pc
import java.{util => ju}

import scala.annotation.switch
import scala.collection.mutable.ListBuffer

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.pc.SemanticTokens._
import scala.meta.pc.VirtualFileParams
import scala.meta.tokens._

import org.eclipse.lsp4j.SemanticTokenModifiers
import org.eclipse.lsp4j.SemanticTokenTypes

/**
 *  Provides semantic tokens of file(@param params)
 *  according to the LSP specification.
 */
final class SemanticTokenProvider(
    protected val cp: MetalsGlobal, // compiler
    val params: VirtualFileParams
) {
  val capableTypes = TokenTypes
  val capableModifiers = TokenModifiers

  // Alias for long notation
  val getTypeId: Map[String, Int] = capableTypes.zipWithIndex.toMap
  val getModifierId: Map[String, Int] = capableModifiers.zipWithIndex.toMap

  // Initialize Tree
  import cp._
  val unit: RichCompilationUnit = cp.addCompilationUnit(
    params.text(),
    params.uri().toString(),
    None
  )
  cp.typeCheck(unit) // initializing unit
  val (root, source) = (unit.lastBody, unit.source)

  def unitPos(offset: Int): Position = unit.position(offset)
  val nodes: List[NodeInfo] = traverser
    .traverse(List.empty[NodeInfo], root)
    .sortBy(_.pos.start)

  /**
   * Main method.  Fist, Codes are convert to Scala.Meta.Tokens.
   * And a semantic token, which is composed by 5 Ints
   * are provided for each meta-token. If a meta-token is
   * Idenitifier, the attributes (e.g. constant or not)
   * are gotten using presentation Compler.
   * All semantic tokens is flattend to a list and returned.
   */
  def provide(): ju.List[Integer] = {

    val buffer = ListBuffer.empty[Integer]

    import scala.meta._
    var cLine = Line(0, 0) // Current Line
    var lastProvided = SingleLineToken(cLine, 0, None)

    for (tk <- params.text().tokenize.get) yield {

      val (tokenType, tokeModifier) = getTypeAndMod(tk)
      var cOffset = tk.pos.start // Current Offset
      var providing = SingleLineToken(cLine, cOffset, Some(lastProvided.copy()))

      // If a meta-Token is over multiline,
      // semantic-token is provided by each line.
      // For ecample, Comment or Literal String.
      for (wkStr <- tk.text.toCharArray.toList.map(c => c.toString)) {
        cOffset += 1
        if (wkStr == "\r") providing.countCR

        // Token Break
        if (wkStr == "\n" | cOffset == tk.pos.end) {
          providing.endOffset =
            if (wkStr == "\n") cOffset - 1
            else cOffset

          if (tokenType != -1) {
            buffer.++=(
              List(
                providing.deltaLine,
                providing.deltaStartChar,
                providing.charSize,
                tokenType,
                tokeModifier
              )
            )
            lastProvided = providing
          }
          // Line Break
          if (wkStr == "\n") {
            cLine = Line(cLine.number + 1, cOffset)
          }
          providing = SingleLineToken(cLine, cOffset, Some(lastProvided.copy()))
        }

      } // end for-wkStr

    } // end for-tk

    buffer.toList.asJava

  }

  // Dealing with single-line semanticToken
  case class Line(
      val number: Int,
      val startOffset: Int
  )
  case class SingleLineToken(
      line: Line, // line which token on
      startOffset: Int, // Offset from start of file.
      lastToken: Option[SingleLineToken]
  ) {
    var endOffset: Int = 0
    var crCount: Int = 0
    def charSize: Int = endOffset - startOffset - crCount
    def deltaLine: Int =
      line.number - this.lastToken.map(_.line.number).getOrElse(0)

    def deltaStartChar: Int = {
      if (deltaLine == 0)
        startOffset - lastToken.map(_.startOffset).getOrElse(0)
      else
        startOffset - line.startOffset
    }
    def countCR: Unit = { crCount += 1 }
  }

  /**
   * This function returns -1 when capable Type is nothing.
   *  TokenTypes that can be on multilines are handled in another func.
   *  See Token.Comment in this file.
   */
  private def typeOfNonIdentToken(
      tk: scala.meta.tokens.Token
  ): Integer = {
    tk match {
      // Alphanumeric keywords
      case _: Token.ModifierKeyword => getTypeId(SemanticTokenTypes.Modifier)
      case _: Token.Keyword => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwNull => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwTrue => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwFalse => getTypeId(SemanticTokenTypes.Keyword)

      // extends Symbolic keywords
      case _: Token.Hash => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.Viewbound => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.LeftArrow => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Subtype => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.RightArrow => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Supertype => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.At => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.Underscore => getTypeId(SemanticTokenTypes.Variable)
      case _: Token.TypeLambdaArrow => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.ContextArrow => getTypeId(SemanticTokenTypes.Operator)

      // Constant
      case _: Token.Constant.Int | _: Token.Constant.Long |
          _: Token.Constant.Float | _: Token.Constant.Double =>
        getTypeId(SemanticTokenTypes.Number)
      case _: Token.Constant.String | _: Token.Constant.Char =>
        getTypeId(SemanticTokenTypes.String)
      case _: Token.Constant.Symbol => getTypeId(SemanticTokenTypes.Property)

      // Comment
      case _: Token.Comment => getTypeId(SemanticTokenTypes.Comment)

      // Interpolation
      case _: Token.Interpolation.Id | _: Token.Interpolation.SpliceStart =>
        getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.Interpolation.Start | _: Token.Interpolation.Part |
          _: Token.Interpolation.SpliceEnd | _: Token.Interpolation.End =>
        getTypeId(SemanticTokenTypes.String) // $ symbol

      // Default
      case _ => -1
    }

  }

  /**
   * The position of @param tk must be incremented from the previous call.
   */
  def pickFromTraversed(tk: scala.meta.tokens.Token): Option[NodeInfo] = {

    val adjustForBacktick: Int = {
      var ret: Int = 0
      val cName = tk.text.toCharArray()
      if (cName.size >= 2) {
        if (
          cName(0) == '`'
          && cName(cName.size - 1) == '`'
        ) ret = 2
      }
      ret
    }

    val nodesIterator = nodes.iterator
    var currentNode = nodesIterator.next()

    def isTarget: Boolean =
      currentNode.pos.start == tk.pos.start &&
        currentNode.pos.end + adjustForBacktick == tk.pos.end

    def continueItelation: Boolean =
      // Goes ahead for appropriate position
      if (currentNode.pos.start < tk.pos.start) true
      // Stops if the position is exceeded
      else if (currentNode.pos.start > tk.pos.start) false
      // Ends successfully if Target is found
      else if (isTarget) false
      // Continues seeking on the appropriate position
      else true

    while (continueItelation && nodesIterator.hasNext)
      currentNode = nodesIterator.next()

    if (isTarget) Some(currentNode)
    else None
  }

  case class NodeInfo(
      sym: Option[Symbol],
      pos: scala.reflect.api.Position
  )
  object NodeInfo {
    def apply(tree: Tree, pos: scala.reflect.api.Position): NodeInfo = {
      val sym = tree.symbol
      if (sym != NoSymbol && sym != null)
        NodeInfo(Some(tree.symbol), pos)
      else NodeInfo(None, pos)
    }

    def apply(sym: Symbol, pos: scala.reflect.api.Position): NodeInfo =
      new NodeInfo(Some(sym), pos)
  }

  /**
   * was written in reference to PcDocumentHighlightProvider.
   */
  import cp._
  object traverser {

    /**
     * gathers all nodes inside given tree.
     * The nodes have symbol.
     */
    def traverse(
        nodes: List[NodeInfo],
        tree: cp.Tree
    ): List[NodeInfo] = {

      tree match {
        /**
         * All indentifiers such as:
         * val a = <<b>>
         */
        case ident: cp.Ident if ident.pos.isRange =>
          val symbol =
            if (ident.symbol == NoSymbol)
              if (ident.tpe != null) {
                ident.tpe.underlying.typeSymbolDirect
              } else {
                val context = doLocateContext(ident.pos)
                context.lookupSymbol(ident.name, _ => true) match {
                  case LookupSucceeded(_, symbol) => symbol
                  case _ => NoSymbol
                }
              }
            else {
              ident.symbol
            }
          nodes :+ NodeInfo(symbol, ident.pos)

        /**
         * Needed for type trees such as:
         * type A = [<<b>>]
         */
        case tpe: cp.TypeTree if tpe.original != null && tpe.pos.isRange =>
          tpe.original.children.foldLeft(
            nodes :+ NodeInfo(tpe.original, typePos(tpe))
          )(traverse(_, _))
        /**
         * All select statements such as:
         * val a = hello.<<b>>
         */
        case sel: cp.Select if sel.pos.isRange =>
          traverse(
            nodes :+ NodeInfo(sel, sel.namePos),
            sel.qualifier
          )
        /**
         * statements such as:
         * val Some(<<a>>) = Some(2)
         */
        case bnd: cp.Bind =>
          bnd.children.foldLeft(nodes :+ NodeInfo(bnd, bnd.pos))(traverse(_, _))

        /* all definitions:
         * def <<foo>> = ???
         * class <<Foo>> = ???
         * etc.
         */
        case df: cp.MemberDef if df.pos.isRange =>
          (annotationChildren(df) ++ df.children)
            .foldLeft(
              nodes :+ NodeInfo(df, df.namePos)
            )(traverse(_, _))
        /* Named parameters, since they don't show up in typed tree:
         * foo(<<name>> = "abc")
         * User(<<name>> = "abc")
         * etc.
         */
        case appl: cp.Apply =>
          val named = appl.args
            .flatMap { arg =>
              namedArgCache.get(arg.pos.start)
            }
            .collect { case cp.AssignOrNamedArg(i @ cp.Ident(_), _) =>
              NodeInfo(
                appl.symbol.paramss.flatten.find(_.name == i.name),
                i.pos
              )
            }

          tree.children.foldLeft(nodes ++ named)(traverse(_, _))

        /**
         * We don't automatically traverser types like:
         * val opt: Option[<<String>>] =
         */
        case tpe: cp.TypeTree if tpe.original != null =>
          tpe.original.children.foldLeft(nodes)(traverse(_, _))
        /**
         * Some type trees don't have symbols attached such as:
         * type A = List[_ <: <<Iterable>>[Int]]
         */
        case id: cp.Ident if id.symbol == cp.NoSymbol =>
          fallbackSymbol(id.name, id.pos) match {
            case Some(_) => nodes :+ NodeInfo(id, id.pos)
            case _ => nodes
          }

        case df: cp.MemberDef =>
          (tree.children ++ annotationChildren(df))
            .foldLeft(nodes)(traverse(_, _))
        /**
         * For traversing import selectors:
         * import scala.util.<<Try>>
         */
        case imp: cp.Import =>
          val ret = for {
            sel <- imp.selectors
          } yield {
            // NodeInfo(symbol, sel.namePosition(source))
            val buffer = ListBuffer.empty[NodeInfo]
            val symbol = imp.expr.symbol.info.member(sel.name)
            buffer.++=(
              List(
                NodeInfo(symbol, sel.namePosition(source))
              )
            )
            def isRename(sel: cp.ImportSelector): Boolean =
              sel.rename != null &&
                sel.rename != nme.WILDCARD &&
                sel.name != sel.rename
            if (isRename(sel)) {
              buffer.++=(
                List(
                  NodeInfo(symbol, sel.renamePosition(source))
                )
              )
            }
            buffer.toList
          }
          traverse(nodes ++ ret.flatten, imp.expr)

        case _ =>
          if (tree == null) null
          else tree.children.foldLeft(nodes)(traverse(_, _))
      }
    }

    def fallbackSymbol(name: cp.Name, pos: cp.Position): Option[Symbol] = {
      val context = cp.doLocateImportContext(pos)
      context.lookupSymbol(name, sym => sym.isType) match {
        case cp.LookupSucceeded(_, symbol) =>
          Some(symbol)
        case _ => None
      }
    }
    private def annotationChildren(mdef: cp.MemberDef): List[cp.Tree] = {
      mdef.mods.annotations match {
        case Nil if mdef.symbol != null =>
          // After typechecking, annotations are moved from the modifiers
          // to the annotation on the symbol of the annotatee.
          mdef.symbol.annotations.map(_.original)
        case anns => anns
      }
    }

    private def typePos(tpe: cp.TypeTree) = {
      tpe.original match {
        case cp.AppliedTypeTree(tpt, _) => tpt.pos
        case sel: cp.Select => sel.namePos
        case _ => tpe.pos
      }
    }
    // We need to collect named params since they will not show on fully typed tree
    lazy private val namedArgCache = {
      val parsedTree = cp.parseTree(source)
      parsedTree.collect { case arg @ AssignOrNamedArg(_, rhs) =>
        rhs.pos.start -> arg
      }.toMap
    }
  }

  def selector(imp: cp.Import, startOffset: Int): Option[cp.Symbol] = {
    for {
      sel <- imp.selectors.reverseIterator
        .find(_.namePos <= startOffset)
    } yield imp.expr.symbol.info.member(sel.name)
  }

  /**
   * returns (SemanticTokenType, SemanticTokenModifier) of @param tk
   */
  private def getTypeAndMod(tk: scala.meta.tokens.Token): (Int, Int) = {

    tk match {
      case ident: Token.Ident => IndentTypeAndMod(ident)
      case _ => (typeOfNonIdentToken(tk), 0)
    }
  }

  /**
   * returns (SemanticTokenType, SemanticTokenModifier) of @param tk
   */
  private def IndentTypeAndMod(ident: Token.Ident): (Int, Int) = {
    val default = (-1, 0)

    val isOperatorName = (ident.name.last: @switch) match {
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' |
          '?' | ':' | '=' | '&' | '|' | '/' | '\\' =>
        true
      case _ => false
    }

    val ret =
      for (
        nodeInfo <- pickFromTraversed(ident);
        sym <- nodeInfo.sym
      ) yield {
        var mod: Int = 0
        def addPwrToMod(tokenID: String) = {
          val place: Int = getModifierId(tokenID)
          if (place != -1) mod += (1 << place)
        }

        // get Type
        val typ =
          if (sym.isValueParameter) getTypeId(SemanticTokenTypes.Parameter)
          else if (sym.isTypeParameter || sym.isTypeSkolem)
            getTypeId(SemanticTokenTypes.TypeParameter)
          else if (isOperatorName) getTypeId(SemanticTokenTypes.Operator)
          // Java Enum
          else if (
            sym.companion
              .hasFlag(scala.reflect.internal.ModifierFlags.JAVA_ENUM)
          )
            getTypeId(SemanticTokenTypes.Enum)
          else if (sym.hasFlag(scala.reflect.internal.ModifierFlags.JAVA_ENUM))
            getTypeId(SemanticTokenTypes.EnumMember)
          // See symbol.keystring about following conditions.
          else if (sym.isJavaInterface)
            getTypeId(SemanticTokenTypes.Interface) // "interface"
          else if (sym.isTrait)
            getTypeId(SemanticTokenTypes.Interface) // "trait"
          else if (sym.isClass) getTypeId(SemanticTokenTypes.Class) // "class"
          else if (sym.isType && !sym.isParameter)
            getTypeId(SemanticTokenTypes.Type) // "type"
          else if (sym.isVariable)
            getTypeId(SemanticTokenTypes.Variable) // "var"
          else if (sym.hasPackageFlag)
            getTypeId(SemanticTokenTypes.Namespace) // "package"
          else if (sym.isModule) getTypeId(SemanticTokenTypes.Class) // "object"
          else if (sym.isSourceMethod)
            if (sym.isGetter | sym.isSetter)
              getTypeId(SemanticTokenTypes.Variable)
            else getTypeId(SemanticTokenTypes.Method) // "def"
          else if (sym.isTerm && (!sym.isParameter || sym.isParamAccessor)) {
            addPwrToMod(SemanticTokenModifiers.Readonly)
            getTypeId(SemanticTokenTypes.Variable) // "val"
          } else -1

        // Modifiers except by ReadOnly
        if (sym.isAbstract) addPwrToMod(SemanticTokenModifiers.Abstract)
        if (sym.isDeprecated) addPwrToMod(SemanticTokenModifiers.Deprecated)
        if (sym.owner.isModule) addPwrToMod(SemanticTokenModifiers.Static)

        (typ, mod)

      }

    ret.getOrElse(default)

  }
}
