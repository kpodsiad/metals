package scala.meta.internal.metals.codeactions

import org.eclipse.lsp4j.CodeActionParams
import org.eclipse.{lsp4j, lsp4j => l}

import scala.concurrent.{ExecutionContext, Future}
import scala.meta.internal.metals.CodeAction
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.parsing.Trees
import scala.meta.io.AbsolutePath
import scala.meta.pc.CancelToken
import scala.meta.{Defn, Term}

/**
 * look at the
 *
 * @param trees
 */
class CreateCompanionObjectCodeAction(
                                       trees: Trees
                                     ) extends CodeAction {
  override def kind: String = l.CodeActionKind.RefactorRewrite

  override def contribute(params: CodeActionParams, token: CancelToken)(implicit
                                                                        ec: ExecutionContext
  ): Future[Seq[l.CodeAction]] = Future {
    val path = params.getTextDocument().getUri().toAbsolutePath
    val range = params.getRange()
    val applyTree =
      if (range.getStart == range.getEnd)
        trees
          .findLastEnclosingAt[Term.Apply](
            path,
            range.getStart(),
            applyWithSingleFunction
          )
      else None

    applyTree.collect { case classDefinition: Defn.Class =>
      findCompanionObjectOfClass(classDefinition) match {
        case Some(comanionObject) =>
          buildShowingCompanionObjectCodeAction(path, comanionObject)
        case None =>
          buildCreatingCompanionObjectCodeAction(path, classDefinition)
      }
    }.toSeq

  }

  def buildCreatingCompanionObjectCodeAction(
                                              path: AbsolutePath,
                                              classDefinition: Defn.Class
                                            ): l.CodeAction = {
    val codeAction = new l.CodeAction()
    codeAction.setTitle(CreateCompanionObjectCodeAction.companionObjectInfo)
    codeAction.setKind(this.kind)
    val rangeStart = classDefinition.pos.toLSP.getEnd
    val rangeEnd = classDefinition.pos.toLSP.getEnd
    rangeEnd.setLine(rangeEnd.getLine + 5)
    val range = new lsp4j.Range(rangeStart, rangeEnd)
    val companionObjectFirstLine = new l.TextEdit(
      range,
      s"""
         |object ${classDefinition.name.value}{}
         |
         |""".stripMargin
    )

    codeAction.setEdit(
      new l.WorkspaceEdit(
        Map(path.toURI.toString -> List(companionObjectFirstLine).asJava).asJava
      )
    )
    codeAction
  }

  def buildShowingCompanionObjectCodeAction(
                                             path: AbsolutePath,
                                             comanionObject: Defn.Object
                                           ): l.CodeAction = {
    val codeAction = new l.CodeAction()
    codeAction.setTitle(CreateCompanionObjectCodeAction.companionObjectInfo)
    val command = new lsp4j.Command()

    codeAction.setKind(this.kind)
    codeAction.setData()
    codeAction
  }

  private def findCompanionObjectOfClass(
                                          classDefinition: Defn.Class
                                        ): Option[Defn.Object] =
    classDefinition.parent.flatMap(_.children.collectFirst {
      case potentialCompanionObject: Defn.Object
        if (potentialCompanionObject.name.value == classDefinition.name.value) =>
        potentialCompanionObject
    })

  private def applyWithSingleFunction: Term.Apply => Boolean = {
    // exclude case when body has more than one line (is a Block) because it cannot be rewritten to parens
    case Term.Apply(
    _,
    List(Term.Block(List(Term.Function(_, _: Term.Block))))
    ) =>
      false
    case Term.Apply(_, List(_: Term)) => true
    //   Term.Apply(_, List(Term.Block(List(_: Term)))) is already included in the one above
    case _ => false
  }
}

object CreateCompanionObjectCodeAction {
  val companionObjectCreation = "Create class companion object"
  val companionObjectInfo = "Show class companion object"
}
