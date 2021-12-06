package tests

import java.util.concurrent.TimeUnit

import scala.concurrent.Future

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ServerCommands
import scala.meta.internal.metals.testProvider.TestSuiteDiscoveryResult

import tests.TestDiscoveryWrapper._

class DiscoverTestSuitesSuite extends BaseLspSuite("discoverTestSuites") {

  test("munit-test-suite") {
    for {
      _ <- Future.successful(cleanWorkspace())
      _ <- initialize(s"""|/metals.json
                          |{
                          |  "app": {
                          |    "libraryDependencies" : [ "org.scalameta::munit:0.7.29", "org.scalatest::scalatest:3.2.1" ],
                          |    "scalaVersion": "2.13.6"
                          |  }
                          |}
                          |/app/src/main/scala/foo/bar/MyTestSuite.scala
                          |package foo.bar
                          |abstract class AbstractTestSuite1 extends munit.FunSuite 
                          |
                          |class MunitTestSuite extends AbstractTestSuite1 {
                          |  test("foo") {}
                          |}
                          |""".stripMargin)
      _ <- server.server.compilations.compilationFinished(
        workspace.resolve("app/src/main/scala/foo/bar/MyTestSuite.scala")
      )
      _ <- server.executeCommand(ServerCommands.CascadeCompile)
      _ <- server.didOpen("app/src/main/scala/foo/bar/MyTestSuite.scala")
      _ <- server.didSave("app/src/main/scala/foo/bar/MyTestSuite.scala")(
        identity
      )
      _ <- server.waitFor(TimeUnit.SECONDS.toMillis(10))
      res <- server.discoverTestSuites(
        "app/src/main/scala/foo/bar/MyTestSuite.scala"
      )
    } yield {
      val obtained = res.map(TestDiscoveryWrapper(_))
      val expected = List(
        TestDiscoveryWrapper(
          "app",
          List(
            PackageWrapper(
              "foo",
              List(
                PackageWrapper(
                  "bar",
                  List(
                    SuiteWrapper(
                      "foo.bar.MunitTestSuite",
                      "MunitTestSuite"
                    )
                  )
                )
              )
            )
          )
        )
      )

      assertEquals(expected, obtained)
    }
  }
}

final case class TestDiscoveryWrapper(
    targetName: String,
    discovered: List[ResultWrapper]
)
object TestDiscoveryWrapper {
  def apply(discovery: TestSuiteDiscoveryResult): TestDiscoveryWrapper =
    TestDiscoveryWrapper(
      discovery.targetName,
      discovery.discovered.asScala
        .map(result => ResultWrapper(result))
        .toList
    )

  sealed trait ResultWrapper
  object ResultWrapper {
    def apply(result: TestSuiteDiscoveryResult.Discovered): ResultWrapper =
      result match {
        case pkg: TestSuiteDiscoveryResult.Package => PackageWrapper(pkg)
        case test: TestSuiteDiscoveryResult.TestSuite => SuiteWrapper(test)
      }
  }

  final case class PackageWrapper private (
      prefix: String,
      children: List[ResultWrapper]
  ) extends ResultWrapper
  object PackageWrapper {
    def apply(pkg: TestSuiteDiscoveryResult.Package): PackageWrapper =
      PackageWrapper(
        pkg.prefix,
        pkg.children.asScala.map(ResultWrapper(_)).toList
      )
  }

  final case class SuiteWrapper private (
      fullyQualifiedName: String,
      className: String
  ) extends ResultWrapper
  object SuiteWrapper {
    def apply(test: TestSuiteDiscoveryResult.TestSuite): SuiteWrapper =
      SuiteWrapper(test.fullyQualifiedName, test.className)
  }
}
