package scala.meta.internal.parsing

import scala.annotation.tailrec

import scala.meta.Defn
import scala.meta.Pkg
import scala.meta.Position
import scala.meta.Tree
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}

final case class ClassWithPos(
    name: String,
    pos: Position
)

class ClassFinder(trees: Trees) {

  def findClass(path: AbsolutePath, pos: l.Position): Option[String] =
    findClass(path, pos, checkInnerClasses = true).filter(_.nonEmpty)

  def findTasty(path: AbsolutePath, pos: l.Position): Option[String] =
    findClass(path, pos, checkInnerClasses = false)
      .filter(_.nonEmpty)
      .map(_.stripSuffix("$"))

  def findAllClasses(
      path: AbsolutePath,
      checkInnerClasses: Boolean
  ): Option[List[ClassWithPos]] = {
    for {
      tree <- trees.get(path)
    } yield {
      val definitions = new scala.collection.mutable.ArrayBuffer[ClassWithPos]()
      var topLevelAdded = false

      def isToplevel(defn: Defn): Boolean =
        !definitions.exists { d =>
          val pos: Position = defn.pos
          val other = d.pos
          other.start <= pos.start && other.end >= pos.start
        }

      def isValid(defn: Defn): Boolean =
        checkInnerClasses || isToplevel(defn)

      tree.traverse {
        case obj: Defn.Object if isValid(obj) =>
          definitions.append(ClassWithPos(s"Object ${obj.name.value}", obj.pos))
        case cls: Defn.Class if isValid(cls) =>
          definitions.append(ClassWithPos(s"Class ${cls.name.value}", cls.pos))
        case trt: Defn.Trait if isValid(trt) =>
          definitions.append(ClassWithPos(s"Trait ${trt.name.value}", trt.pos))
        case dfn: Defn.Def if isToplevel(dfn) && !topLevelAdded =>
          topLevelAdded = true
          definitions.append(ClassWithPos(s"Toplevel package", dfn.pos))
      }
      definitions.toList
    }
  }

  private def findClass(
      path: AbsolutePath,
      pos: l.Position,
      checkInnerClasses: Boolean
  ): Option[String] =
    for {
      tree <- trees.get(path)
    } yield {
      val input = tree.pos.input
      val metaPos = pos.toMeta(input)
      findClassForOffset(tree, metaPos, path.filename, checkInnerClasses)
    }

  private def findClassForOffset(
      tree: Tree,
      pos: Position,
      fileName: String,
      inspectInnerClasses: Boolean
  ): String = {
    @tailrec
    def loop(tree: Tree, symbol: String, isInsideClass: Boolean): String = {
      val delimeter =
        if (symbol.endsWith("$")) ""
        else if (isInsideClass) "$"
        else if (symbol.isEmpty()) ""
        else "."

      val (fullName, isInner) = tree match {
        // toplevel Scala3 definition, generated class is `<filename>$package`
        case _: Defn.Def if !isInsideClass =>
          (
            symbol + delimeter + fileName.stripSuffix(".scala") + "$package",
            false
          )

        case Pkg(ref, _) =>
          val name = ref.toString()
          (symbol + delimeter + name, false)

        case obj: Pkg.Object =>
          val prefix = if (symbol.isEmpty()) "" else "."
          val name = obj.name.toString()
          (symbol + prefix + name + ".package" + "$", true)

        case obj: Defn.Object =>
          val name = obj.name.toString()
          (symbol + delimeter + name + "$", true)

        case cls: Defn.Class =>
          val name = cls.name.toString()
          (symbol + delimeter + name, true)

        case trt: Defn.Trait =>
          val name = trt.name.toString()
          (symbol + delimeter + name, true)

        case _ =>
          (symbol, isInsideClass)
      }

      // Scala 3 outer methods should just return `filename$package`
      // which does not work in case of normal classes
      val shouldNotContinue =
        (tree.is[Defn.Def] && !isInsideClass) ||
          (!inspectInnerClasses && isInsideClass)
      if (shouldNotContinue) {
        fullName
      } else {
        tree.children.find { child =>
          child.pos.start <= pos.start && pos.start <= child.pos.end
        } match {
          case None => fullName
          case Some(t) =>
            loop(t, fullName, isInner)
        }
      }
    }

    loop(tree, "", false)
  }

}
