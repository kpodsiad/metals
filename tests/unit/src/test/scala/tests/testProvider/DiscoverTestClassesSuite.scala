package tests

import scala.meta.internal.metals.ServerCommands

class DiscoverTestClassesSuite extends BaseLspSuite("discoverTests") {

  test("test-classes") {
    for {
      _ <- initialize(s"""|/metals.json
                          |{
                          |  "a": {
                          |    "libraryDependencies" : [ "org.scalameta::munit:0.7.29", "org.scalatest::scalatest:3.2.1" ],
                          |    "scalaVersion": "2.13.5"
                          |  }
                          |}
                          |/a/src/main/scala/foo/bar/Main.scala
                          |package foo.bar
                          |class Main extends App {
                          |
                          |}
                          |/a/src/main/scala/foo/bar/MyTestSuite.scala
                          |package foo.bar
                          |class MyTestSuite extends munit.FunSuite {
                          |  test("foo") {}
                          |}
                          |/a/src/main/scala/foo/bar/MyTestSuite2.scala
                          |package foo.bar
                          |class MyTestSuite2 extends org.scalatest.FunSuite {
                          |  test("foo2") {}
                          |}
                          |
                          |""".stripMargin)
      _ <- server.server.compilations.compilationFinished(
        workspace.resolve("a/src/main/scala/foo/bar/MyTestSuite.scala")
      )
      _ <- server.didOpen("a/src/main/scala/foo/bar/Main.scala")
      _ <- server.didSave("a/src/main/scala/foo/bar/Main.scala")(identity)
      res <- server.executeCommand(ServerCommands.DiscoverTests.id)
    } yield {
      pprint.pprintln(res)
    }
  }

  // def check(
  //     testName: TestOptions,
  //     input: String,
  //     filePath: String
  // ): Unit = {
  //   test(testName) {
  //     for {
  //       _ <- initialize(input)
  //       _ <- server.didOpen(filePath)
  //       res <- server.executeCommand(ServerCommands.DiscoverTests.id)
  //     } yield {
  //       pprint.pprintln(res)
  //     }
  //   }
  // }

}
