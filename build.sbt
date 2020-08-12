import com.softwaremill.Publish.Release.updateVersionInDocs
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._

val scala2_11 = "2.11.12"
val scala2_12 = "2.12.11"
val scala2_13 = "2.13.3"
val scala3 = "0.26.0-RC1"

val scalaTestVersion = "3.2.1"
val scalaNativeTestInterfaceVersion = "0.4.0-M2"
val zioVersion = "1.0.0"
val fs2Version: Option[(Long, Long)] => String = {
  case Some((2, 11)) => "2.1.0"
  case _             => "2.4.2"
}

def dependenciesFor(version: String)(deps: (Option[(Long, Long)] => ModuleID)*): Seq[ModuleID] =
  deps.map(_.apply(CrossVersion.partialVersion(version)))

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.sttp.shared",
  scmInfo := Some(
    ScmInfo(url("https://github.com/softwaremill/sttp-shared"), "scm:git@github.com:softwaremill/sttp-shared.git")
  ),
  // cross-release doesn't work when subprojects have different cross versions
  // work-around from https://github.com/sbt/sbt-release/issues/214
  releaseCrossBuild := false,
  releaseProcess := Seq(
    checkSnapshotDependencies,
    inquireVersions,
    // publishing locally so that the pgp password prompt is displayed early
    // in the process
    releaseStepCommandAndRemaining("publishLocalSigned"),
    releaseStepCommandAndRemaining("clean"),
    releaseStepCommandAndRemaining("test"),
    setReleaseVersion,
    updateVersionInDocs(organization.value),
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("publishSigned"),
    releaseStepCommand("sonatypeBundleRelease"),
    pushChanges
  ),
  // doc generation is broken in dotty
  sources in (Compile, doc) := {
    val scalaV = scalaVersion.value
    val current = (sources in (Compile, doc)).value
    if (scalaV == scala3) Seq() else current
  }
)

val commonJvmSettings = commonSettings ++ Seq(
  scalacOptions ++= Seq("-target:jvm-1.8"),
  scalacOptions := {
    val current = scalacOptions.value
    // https://github.com/lampepfl/dotty/pull/7775
    if (isDotty.value) current ++ List("-language:implicitConversions", "-Ykind-projector") else current
  },
  ideSkipProject := (scalaVersion.value != scala2_13),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

val commonJsSettings = commonSettings ++ Seq(
  // https://github.com/scalaz/scalaz/pull/1734#issuecomment-385627061
  scalaJSLinkerConfig ~= {
    _.withBatchMode(System.getenv("CONTINUOUS_INTEGRATION") == "true")
  },
  scalacOptions in Compile ++= {
    if (isSnapshot.value) Seq.empty
    else
      Seq {
        val dir = project.base.toURI.toString.replaceFirst("[^/]+/?$", "")
        val url = "https://raw.githubusercontent.com/softwaremill/sttp-shared"
        s"-P:scalajs:mapSourceURI:$dir->$url/v${version.value}/"
      }
  },
  ideSkipProject := true,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
  )
)

val commonNativeSettings = commonSettings ++ Seq(
  nativeLinkStubs := true,
  ideSkipProject := true,
  libraryDependencies ++= Seq(
    "org.scala-native" %%% "test-interface" % scalaNativeTestInterfaceVersion,
    "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
  )
)

// run JS tests inside Chrome, due to jsdom not supporting fetch
lazy val browserTestSettings = Seq(
  jsEnv in Test := {
    val debugging = false // set to true to help debugging

    new org.scalajs.jsenv.selenium.SeleniumJSEnv(
      {
        val options = new org.openqa.selenium.chrome.ChromeOptions()
        val args = Seq(
          "auto-open-devtools-for-tabs", // devtools needs to be open to capture network requests
          "no-sandbox",
          "allow-file-access-from-files" // change the origin header from 'null' to 'file'
        ) ++ (if (debugging) Seq.empty else Seq("headless"))
        options.addArguments(args: _*)
        val capabilities = org.openqa.selenium.remote.DesiredCapabilities.chrome()
        capabilities.setCapability(org.openqa.selenium.chrome.ChromeOptions.CAPABILITY, options)
        capabilities
      },
      org.scalajs.jsenv.selenium.SeleniumJSEnv.Config().withKeepAlive(debugging)
    )
  }
)

lazy val projectAggregates: Seq[ProjectReference] = if (sys.env.isDefinedAt("STTP_NATIVE")) {
  println("[info] STTP_NATIVE defined, including sttp-native in the aggregate projects")
  core.projectRefs ++ model.projectRefs ++ akka.projectRefs ++ fs2.projectRefs ++ monix.projectRefs ++ zio.projectRefs
} else {
  println("[info] STTP_NATIVE *not* defined, *not* including sttp-native in the aggregate projects")
  List(
    core.jvm(scala2_11),
    core.jvm(scala2_12),
    core.jvm(scala2_13),
    core.jvm(scala3),
    core.js(scala2_11),
    core.js(scala2_12),
    core.js(scala2_13),
    model.jvm(scala2_11),
    model.jvm(scala2_12),
    model.jvm(scala2_13),
    model.jvm(scala3),
    model.js(scala2_11),
    model.js(scala2_12),
    model.js(scala2_13),
    akka.jvm(scala2_12),
    akka.jvm(scala2_13),
    fs2.jvm(scala2_11),
    fs2.jvm(scala2_12),
    fs2.jvm(scala2_13),
    monix.jvm(scala2_11),
    monix.jvm(scala2_12),
    monix.jvm(scala2_13),
    monix.js(scala2_12),
    monix.js(scala2_13),
    zio.jvm(scala2_11),
    zio.jvm(scala2_12),
    zio.jvm(scala2_13),
    zio.jvm(scala3),
  )
}

val compileAndTest = "compile->compile;test->test"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(skip in publish := true, name := "sttp-shared")
  .aggregate(projectAggregates: _*)

lazy val core = (projectMatrix in file("core"))
  .settings(
    name := "core"
  )
  .jvmPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13, scala3),
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13),
    settings = commonJsSettings ++ browserTestSettings
  )
  .nativePlatform(
    scalaVersions = List(scala2_11),
    settings = commonNativeSettings
  )

lazy val model = (projectMatrix in file("model"))
  .settings(
    name := "model"
  )
  .jvmPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13, scala3),
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13),
    settings = commonJsSettings ++ browserTestSettings
  )
  .nativePlatform(
    scalaVersions = List(scala2_11),
    settings = commonNativeSettings
  )

lazy val akka = (projectMatrix in file("akka"))
  .settings(
    name := "akka"
  )
  .jvmPlatform(
    scalaVersions = List(scala2_12, scala2_13),
    settings = commonJvmSettings ++ Seq(
      libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.8" % "provided"
    )
  )
  .dependsOn(core)

lazy val fs2 = (projectMatrix in file("fs2"))
  .settings(
    name := "fs2",
    libraryDependencies ++= dependenciesFor(scalaVersion.value)(
      "co.fs2" %% "fs2-io" % fs2Version(_)
    )
  )
  .jvmPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13),
    settings = commonJvmSettings
  )
  .dependsOn(core)

lazy val monix = (projectMatrix in file("monix"))
  .settings(
    name := "monix",
    libraryDependencies += "io.monix" %%% "monix" % "3.2.2"
  )
  .jvmPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13, scala3),
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = List(scala2_12, scala2_13),
    settings = commonJsSettings ++ browserTestSettings
  )
  .dependsOn(core)

lazy val zio = (projectMatrix in file("zio"))
  .settings(
    name := "zio",
    libraryDependencies ++= Seq("dev.zio" %% "zio-streams" % zioVersion, "dev.zio" %% "zio" % zioVersion)
  )
  .jvmPlatform(
    scalaVersions = List(scala2_11, scala2_12, scala2_13, scala3),
    settings = commonJvmSettings
  )
  .dependsOn(core)
