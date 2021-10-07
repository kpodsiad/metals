package scala.meta.internal.metals.testProvider

import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.debug.BuildTargetClasses

final case class TestDiscoveryResult(
    target: Any,
    classes: java.util.List[String]
)

final class TestProvider(
    buildTargets: BuildTargets,
    buildTargetClasses: BuildTargetClasses
    // clientConfig: ClientConfiguration,
) {

  def testClasses(): Seq[TestDiscoveryResult] = {
    pprint.pprintln(buildTargets.allBuildTargetIds)
    buildTargets.allBuildTargetIds.toList
      .map { buildTarget =>
        pprint.pprintln(
          buildTargetClasses.classesOf(buildTarget).mainClasses.toList
        )
        pprint.pprintln(
          buildTargetClasses.classesOf(buildTarget).testClasses.toList
        )
        TestDiscoveryResult(
          buildTarget,
          buildTargetClasses
            .classesOf(buildTarget)
            .testClasses
            .values
            .toList
            .asJava
        )
      }
  }

}
