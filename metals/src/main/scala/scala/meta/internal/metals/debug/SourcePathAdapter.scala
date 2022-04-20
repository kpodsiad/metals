package scala.meta.internal.metals.debug

import java.io.File
import java.net.URI
import java.nio.file.Paths

import scala.util.Try

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.URIEncoderDecoder
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import scala.util.Failure
import scala.util.Success

final class SourcePathAdapter(
    workspace: AbsolutePath,
    sources: Set[AbsolutePath],
    buildTargets: BuildTargets,
    supportVirtualDocuments: Boolean
) {
  // when virtual documents are supported there is no need to save jars on disk
  private val saveJarFileToDisk = !supportVirtualDocuments
  private val dependencies = workspace.resolve(Directories.dependencies)

  def toDapURI(sourcePath: AbsolutePath): Option[URI] = {
    if (sources.contains(sourcePath)) Some(sourcePath.toURI)
    else if (supportVirtualDocuments) {
      Try {
        pprint.log(workspace)
        pprint.log(sourcePath)
        val sourceFileJarPath = workspace.toNIO
          .relativize(sourcePath.toNIO)
          .toString
          .stripPrefix("jar:")
        val x = URIEncoderDecoder.decode(sourceFileJarPath)
        pprint.log(x)
        val decodedPath =
          x.toAbsolutePath
        val parts =
          decodedPath.toNIO.iterator().asScala.map(_.toString).toVector
        val jarPartIndex = parts.indexWhere(path => path.endsWith(".jar!"))
        val jarPath = AbsolutePath(
          parts
            .take(jarPartIndex + 1)
            .mkString(File.separator, File.separator, "")
            .stripSuffix("!")
        )
        val relative = parts.drop(jarPartIndex + 1).mkString(File.separator)

        pprint.log(relative)
        val sourceURI =
          FileIO.withJarFileSystem(jarPath, create = false)(root =>
            root.resolve(relative).toURI
          )
        sourceURI
      } match {
        case Failure(exception) =>
          exception.printStackTrace()
          None
        case Success(value) =>
          pprint.log(value.toString())
          Some(value)
      }
    } else {
      // if sourcePath is a dependency source file
      // we retrieve the original source jar and we build the uri innside the source jar filesystem
      for {
        dependencySource <- sourcePath.toRelativeInside(dependencies)
        dependencyFolder <- dependencySource.toNIO.iterator.asScala.headOption
        jarName = dependencyFolder.toString
        jarFile <- buildTargets.sourceJarFile(jarName)
        relativePath <- sourcePath.toRelativeInside(
          dependencies.resolve(jarName)
        )
      } yield FileIO.withJarFileSystem(jarFile, create = false)(root =>
        root.resolve(relativePath.toString).toURI
      )
    }
  }

  def toMetalsPath(sourcePath: String): Option[AbsolutePath] = try {
    val sourceUri =
      Try(URI.create(sourcePath)).getOrElse(Paths.get(sourcePath).toUri())
    sourceUri.getScheme match {
      case "jar" =>
        val path = sourceUri.toAbsolutePath
        Some(if (saveJarFileToDisk) path.toFileOnDisk(workspace) else path)
      case "file" => Some(AbsolutePath(Paths.get(sourceUri)))
      case _ => None
    }
  } catch {
    case e: Throwable =>
      scribe.error(s"Could not resolve $sourcePath", e)
      None
  }
}

object SourcePathAdapter {
  def apply(
      buildTargets: BuildTargets,
      targets: Seq[BuildTargetIdentifier],
      supportVirtualDocuments: Boolean
  ): SourcePathAdapter = {
    val workspace = buildTargets.workspaceDirectory(targets.head).get
    val sources =
      targets.flatMap(buildTargets.buildTargetTransitiveSources).toSet
    new SourcePathAdapter(
      workspace,
      sources,
      buildTargets,
      supportVirtualDocuments
    )
  }
}
