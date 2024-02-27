import com.softwaremill.SbtSoftwareMillBrowserTestJS._
import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.ossPublishSettings

val scala2_12 = "2.12.18"
val scala2_13 = "2.13.13"
val scala2 = List(scala2_12, scala2_13)
val scala3 = List("3.3.1")

val scalaTestVersion = "3.2.18"

excludeLintKeys in Global ++= Set(ideSkipProject)

def dependenciesFor(version: String)(deps: (Option[(Long, Long)] => ModuleID)*): Seq[ModuleID] =
  deps.map(_.apply(CrossVersion.partialVersion(version)))

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.sttp.model",
  mimaPreviousArtifacts := Set.empty,
  versionScheme := Some("semver-spec")
)

val commonJvmSettings = commonSettings ++ Seq(
  scalacOptions ++= Seq("-target:jvm-1.8"),
  ideSkipProject := (scalaVersion.value != scala2_13),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  ),
  mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
  mimaReportBinaryIssues := { if ((publish / skip).value) {} else mimaReportBinaryIssues.value }
)

val commonJsSettings = commonSettings ++ Seq(
  ideSkipProject := true,
  Compile / scalacOptions ++= {
    if (isSnapshot.value) Seq.empty
    else
      Seq {
        val mapSourcePrefix =
          if (ScalaArtifacts.isScala3(scalaVersion.value))
            "-scalajs-mapSourceURI"
          else
            "-P:scalajs:mapSourceURI"
        val dir = project.base.toURI.toString.replaceFirst("[^/]+/?$", "")
        val url = "https://raw.githubusercontent.com/softwaremill/sttp-model"
        s"$mapSourcePrefix:$dir->$url/v${version.value}/"
      }
  },
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
  )
)

val commonNativeSettings = commonSettings ++ Seq(
  ideSkipProject := true,
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
  )
)

lazy val rawAllAggregates: Seq[ProjectReference] = core.projectRefs

lazy val allAggregates: Seq[ProjectReference] = {
  if (sys.env.isDefinedAt("STTP_NATIVE")) {
    println("[info] STTP_NATIVE defined, including native in the aggregate projects")
    rawAllAggregates
  } else {
    println("[info] STTP_NATIVE *not* defined, *not* including native in the aggregate projects")
    rawAllAggregates.filterNot(_.toString.contains("Native"))
  }
}

val compileAndTest = "compile->compile;test->test"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish / skip := true, name := "sttp-model", scalaVersion := scala2_13)
  .aggregate(allAggregates: _*)

lazy val core = (projectMatrix in file("core"))
  .settings(
    name := "core"
  )
  .jvmPlatform(
    scalaVersions = scala2 ++ scala3,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scala2 ++ scala3,
    settings = commonJsSettings ++ browserChromeTestSettings ++ Seq(
      libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0"
    )
  )
  .nativePlatform(
    scalaVersions = scala2 ++ scala3,
    settings = commonNativeSettings
  )

lazy val benchmark = (projectMatrix in file("benchmark"))
  .settings(
    name := "benchmark",
    publish / skip := true
  )
  .jvmPlatform(
    scalaVersions = scala2,
    settings = commonJvmSettings
  )
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
