package tests

import java.util.Collections.emptyList

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.Trace
import scala.meta.internal.metals.debug.DebugProtocol
import scala.meta.internal.metals.debug.DebugStep._
import scala.meta.internal.metals.debug.DebugWorkspaceLayout
import scala.meta.internal.metals.debug.StepNavigator
import scala.meta.internal.metals.debug.Stoppage
import scala.meta.internal.metals.debug.TestDebugger

import ch.epfl.scala.bsp4j.DebugSessionParamsDataKind
import ch.epfl.scala.bsp4j.ScalaMainClass
import munit.GenericBeforeEach
import munit.Location
import munit.TestOptions
import org.eclipse.lsp4j.debug.SetBreakpointsResponse
import scala.meta.internal.metals.debug.SourcePathAdapter
import scala.meta.io.AbsolutePath
import org.eclipse.lsp4j.debug.SourceBreakpoint
import tests.DapTestEnrichments._
import org.eclipse.lsp4j.debug.Source
abstract class BaseDapSuite(
    suiteName: String,
    initializer: BuildServerInitializer,
    buildToolLayout: BuildToolLayout
) extends BaseLspSuite(suiteName, initializer) {

  private val dapClient = Trace.protocolTracePath(DebugProtocol.clientName)
  private val dapServer = Trace.protocolTracePath(DebugProtocol.serverName)

  override def beforeEach(context: GenericBeforeEach[Future[Any]]): Unit = {
    super.beforeEach(context)
    dapClient.touch()
    dapServer.touch()
  }

  protected def logDapTraces(): Unit = {
    if (isCI) {
      scribe.warn("The DAP test failed, printing the traces")
      scribe.warn(dapClient.toString() + ":\n" + dapClient.readText)
      scribe.warn(dapServer.toString() + ":\n" + dapServer.readText)
    }
  }

  override def munitTestTransforms: List[TestTransform] =
    super.munitTestTransforms :+
      new TestTransform(
        "Print DAP traces",
        { test =>
          test.withBody(() =>
            test
              .body()
              .andThen {
                case Failure(exception) =>
                  logDapTraces()
                  exception
                case Success(value) => value
              }(munitExecutionContext)
          )
        }
      )

  def debugMain(
      buildTarget: String,
      main: String,
      stoppageHandler: Stoppage.Handler = Stoppage.Handler.Continue
  ): Future[TestDebugger] = {
    val kind = DebugSessionParamsDataKind.SCALA_MAIN_CLASS
    val mainClass = new ScalaMainClass(main, emptyList(), emptyList())
    server.startDebugging(buildTarget, kind, mainClass, stoppageHandler)
  }

  def setBreakpoints(
      debugger: TestDebugger,
      workspace: DebugWorkspaceLayout
  ): Future[List[SetBreakpointsResponse]] = {
    Future.sequence {
      workspace.files
        .filter(_.breakpoints.nonEmpty)
        .map { file =>
          val path = server.toPath(file.relativePath)
          debugger.setBreakpoints(
            path.toDAP,
            file.breakpoints.map(_.toBreakpoint).toVector
          )
        }
    }
  }

  def setBreakpointsInDependencies(
      debugger: TestDebugger,
      dependencyBreakpoints: Map[String, Vector[Int]]
  ): Future[Vector[SetBreakpointsResponse]] = {
    val adapter = SourcePathAdapter(
      server.server.buildTargets,
      server.server.buildTargets.all.map(_.getId).toVector,
      true
    )
    Future.sequence {
      dependencyBreakpoints.map { case (file, lines) =>
        val path = AbsolutePath(file)
        val adaptedPathOpt = adapter.toDapURI(path)
        assert(
          adaptedPathOpt.isDefined,
          "SourcePathAdapter couldn't adapt path"
        )
        val adaptedPath = adaptedPathOpt.get
        val source = new Source
        source.setName("List.scala")
        source.setPath(adaptedPath.toString())
        pprint.log(adaptedPath)
        val breakpoints = lines.map { line =>
          val breakpoint = new SourceBreakpoint
          breakpoint.setLine(line)
          breakpoint
        }
        debugger.setBreakpoints(source, breakpoints)
      }.toVector
    }
  }

  def removeBreakpoints(
      debugger: TestDebugger,
      workspace: DebugWorkspaceLayout
  ): Future[List[SetBreakpointsResponse]] = {
    Future.sequence {
      workspace.files
        .filter(_.breakpoints.nonEmpty)
        .map { file =>
          val path = server.toPath(file.relativePath)
          debugger.setBreakpoints(path.toDAP, Vector.empty)
        }
    }
  }

  def scalaVersion = BuildInfo.scalaVersion

  def assertBreakpoints(
      name: TestOptions,
      main: Option[String] = None
  )(
      source: String,
      dependencyBreakpoints: Map[String, Vector[Int]] = Map.empty
  )(implicit loc: Location): Unit = {
    test(name) {

      cleanWorkspace()
      val debugLayout = DebugWorkspaceLayout(source)
      val workspaceLayout = buildToolLayout(debugLayout.toString, scalaVersion)
      val navigator = navigateExpectedBreakpoints(debugLayout)

      for {
        _ <- initialize(workspaceLayout)
        _ = assertNoDiagnostics()
        debugger <- debugMain("a", main.getOrElse("a.Main"), navigator)
        _ <- debugger.initialize
        _ <- debugger.launch
        r <- setBreakpoints(debugger, debugLayout)
        _ = pprint.log(r)
        result <- setBreakpointsInDependencies(debugger, dependencyBreakpoints)
        _ = pprint.log(result)
        _ <- debugger.configurationDone
        _ <- debugger.shutdown
      } yield ()
    }
  }

  def navigateExpectedBreakpoints(
      workspaceLayout: DebugWorkspaceLayout
  ): StepNavigator = {

    val expectedBreakpoints = workspaceLayout.files.flatMap { file =>
      file.breakpoints.map(b => Breakpoint(file.relativePath, b.startLine))
    }

    expectedBreakpoints.foldLeft(StepNavigator(workspace)) {
      (navigator, breakpoint) =>
        navigator.at(breakpoint.relativePath, breakpoint.line + 1)(Continue)
    }
  }

}

private final case class Breakpoint(relativePath: String, line: Int)
