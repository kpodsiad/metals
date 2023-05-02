package tests

import scala.meta.internal.metals.DebugUnresolvedMainClassParams
import scala.meta.internal.metals.{BuildInfo => V}
import scala.concurrent.Future
import scala.meta.internal.metals.debug.TestDebugger

class ScalaCliSuite extends tests.BaseScalaCliSuite(V.scala213) {

  import scala.meta.internal.metals.JsonParser._

  def startDebugging(
      main: String,
      buildTarget: String,
  ): Future[TestDebugger] = {
    server.startDebuggingUnresolved(
      new DebugUnresolvedMainClassParams(main, buildTarget).toJson
    )
  }

  test("base-native-run") {
    cleanWorkspace()
    for {
      _ <- scalaCliInitialize(useBsp = true)(
        s"""/MyMain.scala
           |//> using scala "$scalaVersion"
           |//> using platform "native"
           |
           |object MyMain {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello world!")
           |    System.exit(0)
           |  }
           |}
           |
           |""".stripMargin
      )
      _ <- server.didOpen("MyMain.scala")
      targets <- server.listBuildTargets
      mainTarget = targets.find(!_.contains("test"))
      _ = assert(mainTarget.isDefined, "No main target specified")
      debugServer <- startDebugging("MyMain", mainTarget.get)
      _ <- debugServer.initialize
      _ <- debugServer.launch
      _ <- debugServer.configurationDone
      _ <- debugServer.shutdown
      output <- debugServer.allOutput
    } yield assertContains(output, "Hello world!\n")
  }
}
