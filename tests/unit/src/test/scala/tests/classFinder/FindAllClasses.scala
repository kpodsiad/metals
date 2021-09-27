package tests
package classFinder

import java.nio.file.Paths

import scala.meta.internal.metals.{BuildInfo => V}
import scala.meta.io.AbsolutePath

import munit.TestOptions

class FindAllClasses extends BaseClassFinderSuite {

  check(
    "only-toplevel",
    """|package a
       |class Foo 
       |object Foo
       |class Bar {
       |  val x = 155
       |  class InsideBar
       |  def xx2() = {
       |    class InsideMethod
       |  }
       |}
       |def foo(): Unit = ()
       |def foo2(): Unit = ()
       |""".stripMargin,
    s"""|Class Foo (1, 0)
        |Object Foo (2, 0)
        |Class Bar (3, 0)
        |Toplevel package (10, 0)""".stripMargin,
    scalaVersion = V.scala3
  )

  check(
    "all",
    """|package a
       |class Foo 
       |object Foo
       |class Bar {
       |  val x = 155
       |  class InsideBar
       |  def xx2() = {
       |    class InsideMethod
       |  }
       |}
       |def foo(): Unit = ()
       |def foo2(): Unit = ()
       |""".stripMargin,
    s"""|Class Foo (1, 0)
        |Object Foo (2, 0)
        |Class Bar (3, 0)
        |Class InsideBar (5, 2)
        |Class InsideMethod (7, 4)
        |Toplevel package (10, 0)""".stripMargin,
    checkInnerClasses = true,
    scalaVersion = V.scala3
  )

  def check(
      name: TestOptions,
      sourceText: String,
      expected: String,
      checkInnerClasses: Boolean = false,
      filename: String = "Main.scala",
      scalaVersion: String = V.scala213
  ): Unit =
    test(name) {
      val (buffers, classFinder) = init(scalaVersion)
      val path = AbsolutePath(Paths.get(filename))
      buffers.put(path, sourceText)
      val classes = classFinder.findAllClasses(path, checkInnerClasses)

      assert(classes.isDefined)
      assertEquals(
        classes.get
          .map(c => s"${c.name} (${c.pos.startLine}, ${c.pos.startColumn})")
          .mkString("\n"),
        expected
      )
    }

}
