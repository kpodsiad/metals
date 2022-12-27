package scala.meta.metals

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.util.control.NonFatal

import scala.meta.internal.bsp.BspServers
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.ClasspathSearch
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.MetalsLanguageServer
import scala.meta.internal.metals.MetalsServerConfig
import scala.meta.internal.metals.MtagsResolver
import scala.meta.internal.metals.MutableCancelable
import scala.meta.internal.metals.ProgressTicks
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.clients.language.MetalsLanguageClient
import scala.meta.internal.metals.clients.language.NoopLanguageClient
import scala.meta.internal.metals.logging.LanguageClientLogger
import scala.meta.internal.metals.logging.MetalsLogger
import scala.meta.io.AbsolutePath
import scala.meta.metals.ServerState.ShuttingDown
import scala.meta.metals.lsp.DelegatingService
import scala.meta.metals.lsp.LanguageServer
import scala.meta.metals.lsp.TextDocumentAndWorkspaceService

import org.eclipse.lsp4j._

/**
 * According to the spec, the server waits for the `initialize` request to be
 * sent. After that, the server sends the `initialized` notification to the
 * client. Next, the server is fully working and can receive requests and
 * notifications from the client.
 */
sealed trait ServerState
object ServerState {
  case object Started extends ServerState
  final case class Initialized(server: MetalsLanguageServer) extends ServerState
  final case class ShuttingDown(server: MetalsLanguageServer)
      extends ServerState
  case object Exited extends ServerState
}

/**
 * Scala Language Server implementation.
 */
class MetalsNewLanguageServer(
    ec: ExecutionContextExecutorService,
    buffers: Buffers = Buffers(),
    redirectSystemOut: Boolean = true,
    charset: Charset = StandardCharsets.UTF_8,
    time: Time = Time.system,
    initialConfig: MetalsServerConfig = MetalsServerConfig.default,
    progressTicks: ProgressTicks = ProgressTicks.braille,
    bspGlobalDirectories: List[AbsolutePath] =
      BspServers.globalInstallDirectories,
    sh: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(),
    isReliableFileWatcher: Boolean = true,
    mtagsResolver: MtagsResolver = MtagsResolver.default(),
    onStartCompilation: () => Unit = () => (),
    classpathSearchIndexer: ClasspathSearch.Indexer =
      ClasspathSearch.Indexer.default,
) extends LanguageServer {

  private implicit val executionContext: ExecutionContextExecutorService = ec

  // AtomicReference's set and get have the same semantics as volatile variables.
  // because we only use get and set, there is no need to use AtomicReference.
  @volatile
  private var serverState: ServerState = ServerState.Started
  @volatile
  private var languageClient: MetalsLanguageClient = NoopLanguageClient

  private val cancelables = new MutableCancelable()
  val isCancelled = new AtomicBoolean(false)

  private val metalsService = new DelegatingService(
    new TextDocumentAndWorkspaceService {}
  )

  def connectToLanguageClient(languageClient: MetalsLanguageClient): Unit = {
    this.languageClient = languageClient
    LanguageClientLogger.languageClient = Some(languageClient)
    cancelables.add(() => languageClient.shutdown())
  }

  def cancelAll(): Unit = {
    if (isCancelled.compareAndSet(false, true)) {
      cancelables.cancel()
      serverState match {
        case ServerState.Initialized(server) => server.cancelAll()
        case ShuttingDown(server) => server.cancelAll()
        case _ =>
          scribe.warn(
            s"Server is in state $serverState, cannot invoke cancelAll"
          )
      }
    }
  }

  override def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] =
    if (serverState != ServerState.Started) {
      Future
        .failed[InitializeResult](
          new IllegalStateException(
            s"Server state is ${serverState}, expected ${ServerState.Started}"
          )
        )
        .asJava
    } else {
      // NOTE: we purposefully don't check workspaceFolders here
      // since Metals technically doesn't support it. Once we implement
      // https://github.com/scalameta/metals-feature-requests/issues/87 we'll
      // have to change this.
      val root =
        Option(params.getRootUri())
          .orElse(Option(params.getRootPath()))
          .map(_.toAbsolutePath)
      root match {
        case None =>
          languageClient.showMessage(Messages.noRoot)
          Future
            .failed(
              new IllegalArgumentException(
                "There is no root directory in InitializeParams"
              )
            )
            .asJava
        case Some(workspace) =>
          val server = new MetalsLanguageServer(
            ec = ec,
            buffers = buffers,
            workspace = workspace,
            client = languageClient,
            initializeParams = params,
            charset = charset,
            time = time,
            initialConfig = initialConfig,
            progressTicks = progressTicks,
            bspGlobalDirectories = bspGlobalDirectories,
            sh = sh,
            isReliableFileWatcher = isReliableFileWatcher,
            mtagsResolver = mtagsResolver,
            onStartCompilation = onStartCompilation,
            classpathSearchIndexer = classpathSearchIndexer,
          )
          MetalsLogger.setupLspLogger(workspace, redirectSystemOut)
          serverState = ServerState.Initialized(server)
          metalsService.underlying = server
          server.initialize(params)
      }
    }

  private val isInitialized = new AtomicBoolean(false)

  override def initialized(
      params: InitializedParams
  ): CompletableFuture[Unit] = {
    // Avoid duplicate `initialized` notifications. During the transition
    // for https://github.com/natebosch/vim-lsc/issues/113 to get fixed,
    // we may have users on a fixed vim-lsc version but with -Dmetals.no-initialized=true
    // enabled.
    if (isInitialized.compareAndSet(false, true)) {
      pprint.log("initialized")
      serverState match {
        case ServerState.Initialized(server) =>
          server.initialized()
        case _ =>
          Future.failed(new Exception)
      }
    } else {
      scribe.warn("Ignoring duplicate 'initialized' notification.")
      Future.unit
    }
  }.recover { case NonFatal(e) =>
    scribe.error("Unexpected error initializing server", e)
  }.asJava

  override def shutdown(): CompletableFuture[Unit] = serverState match {
    case ServerState.Initialized(server) =>
      server.shutdown()
    case _ =>
      Future.unit.asJava
  }

  override def exit(): Unit = serverState match {
    case ServerState.ShuttingDown(server) =>
      server.exit()
    case _ => ()
  }

  override val getTextDocumentAndWorkspaceService
      : TextDocumentAndWorkspaceService = metalsService

  @deprecated
  def getOldMetalsLanguageServer: MetalsLanguageServer = serverState match {
    case ServerState.Initialized(server) => server
    case _ => throw new IllegalStateException("Server is not initialized")
  }

}
