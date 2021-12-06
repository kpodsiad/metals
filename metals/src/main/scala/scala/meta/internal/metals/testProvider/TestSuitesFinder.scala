package scala.meta.internal.metals.testProvider

import scala.meta.internal.implementation.ImplementationProvider
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.debug.BuildTargetClasses

import ch.epfl.scala.bsp4j.BuildTarget

trait TestSuitesFinder {
  def findTestSuites(): Seq[TestSuiteDiscoveryResult]
}

final class TestSuitesFinderImpl(
    buildTargets: BuildTargets,
    buildTargetClasses: BuildTargetClasses,
    definitionProvider: DefinitionProvider,
    implementationProvider: ImplementationProvider
) extends TestSuitesFinder {

  override def findTestSuites(): Seq[TestSuiteDiscoveryResult] = {
    buildTargets.allBuildTargetIds.toList
      .flatMap(buildTargets.info)
      .filterNot(_.getDisplayName.endsWith("build"))
      .map(buildTarget => createTestDiscoveryFor(buildTarget))
      // don't send empty results
      .filter(_.discovered.asScala.exists(_.nonEmpty))
  }

  private def createTestDiscoveryFor(buildTarget: BuildTarget) = {
    val classes = buildTargetClasses
      .classesOf(buildTarget.getId)
      .testClasses
      // filter out symbols which don't represent classes
      .filter { case (symbol, _) => symbol.endsWith("#") }
      .toList
      .map { case (symbol, fullyQualifiedClassName) =>
        createTestEntry(buildTarget, symbol, fullyQualifiedClassName)
      }

    TestSuiteDiscoveryResult(
      buildTarget.getDisplayName,
      buildTarget.getId.getUri,
      TestProviderImpl.groupTestsByPackage(classes).asJava
    )
  }

  private def createTestEntry(
      buildTarget: BuildTarget,
      symbol: String,
      fullyQualifiedClassName: String
  ): TestEntry = {
    val location = for {
      definition <- definitionProvider
        .toPath(symbol, List(buildTarget.getId))
      location <- definition.toResult.flatMap(
        _.locations.asScala.toList
          .filter(_.getUri == definition.uri)
          .headOption
      )
    } yield location
    // fullyQualifiedClassName always contains at least one element - class name
    val fullyQualifiedName = fullyQualifiedClassName.split('.').toList
    val testClass = TestSuiteDiscoveryResult.TestSuite(
      fullyQualifiedClassName,
      fullyQualifiedName.takeRight(1).head,
      location.orNull
    )
    TestEntry(
      fullyQualifiedName.dropRight(1),
      testClass
    )
  }
}

private case class TestEntry(
    packageParts: List[String],
    testClass: TestSuiteDiscoveryResult.TestSuite
) {
  def stripPackage(): TestEntry =
    this.copy(packageParts = this.packageParts.drop(1))
}

object TestProviderImpl {
  def groupTestsByPackage(
      testEntries: List[TestEntry]
  ): List[TestSuiteDiscoveryResult.Discovered] = groupTestsByPackageImpl(
    testEntries
  )

  /**
   * Partitions the given testEntries depending on whether entry has nonempty package or not.
   * Mapped those empty ones into TestClass, strip package for nonempty ones and repeat process for them.
   */
  private def groupTestsByPackageImpl(
      testEntries: List[TestEntry]
  ): List[TestSuiteDiscoveryResult.Discovered] = {
    val (withPackage, withoutPackage) =
      testEntries.partition(entry => entry.packageParts.nonEmpty)
    val currentTestClasses = withoutPackage.map(_.testClass)
    val testClassesInPackages: Iterable[TestSuiteDiscoveryResult.Discovered] =
      withPackage
        .groupBy(entry => entry.packageParts.head)
        .mapValues(
          _.map(entry => entry.copy(packageParts = entry.packageParts.drop(1)))
        )
        .map { case (prefix, entries) =>
          val children = groupTestsByPackageImpl(entries)
          TestSuiteDiscoveryResult.Package(prefix, children.asJava)
        }
    val result = currentTestClasses ++ testClassesInPackages
    result
  }
}
