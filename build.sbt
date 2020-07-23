import sbtrelease.ReleaseStateTransformations._
import sbtrelease.ReleasePlugin.autoImport._
import com.softwaremill.Publish.Release.updateVersionInDocs

val scala2_11 = "2.11.12"
val scala2_12 = "2.12.11"
val scala2_13 = "2.13.3"
val scala3 = "0.24.0"

lazy val is2_11 = settingKey[Boolean]("Is the scala version 2.11.")

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.sttp.model",
  // cross-release doesn't work when subprojects have different cross versions
  // work-around from https://github.com/sbt/sbt-release/issues/214
  releaseProcess := Seq(
    checkSnapshotDependencies,
    inquireVersions,
    // publishing locally so that the pgp password prompt is displayed early
    // in the process
    releaseStepCommandAndRemaining("+publishLocalSigned"),
    releaseStepCommandAndRemaining("+clean"),
    releaseStepCommandAndRemaining("+test"),
    setReleaseVersion,
    updateVersionInDocs(organization.value),
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("+publishSigned"),
    releaseStepCommand("sonatypeBundleRelease"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  ),
  is2_11 := scalaVersion.value.startsWith("2.11.")
)

// an ugly work-around for https://github.com/sbt/sbt/issues/3465
// even if a project is 2.11-only, we fake that it's also 2.12/2.13-compatible
val only2_11settings = Seq(
  publishArtifact := is2_11.value,
  skip := !is2_11.value,
  skip in compile := !is2_11.value,
  skip in publish := !is2_11.value,
  libraryDependencies := (if (is2_11.value) libraryDependencies.value else Nil)
)

val commonJvmJsSettings = commonSettings ++ Seq(
  scalaVersion := scala2_11,
  crossScalaVersions := Seq(scalaVersion.value, scala2_12, scala2_13)
)

val commonJvmSettings = commonJvmJsSettings ++ Seq(
  scalacOptions ++= Seq("-target:jvm-1.8"),
  crossScalaVersions += scala3,
  sources in (Compile, doc) := {
    val scalaV = scalaVersion.value
    val current = (sources in (Compile, doc)).value
    if (scalaV == scala3) Seq() else current
  }
)

val commonJsSettings = commonJvmJsSettings ++ Seq(
  // https://github.com/scalaz/scalaz/pull/1734#issuecomment-385627061
  scalaJSLinkerConfig ~= {
    _.withBatchMode(System.getenv("CONTINUOUS_INTEGRATION") == "true")
  },
  scalacOptions in Compile ++= {
    if (isSnapshot.value) Seq.empty
    else
      Seq {
        val dir = project.base.toURI.toString.replaceFirst("[^/]+/?$", "")
        val url = "https://raw.githubusercontent.com/softwaremill/sttp-model"
        s"-P:scalajs:mapSourceURI:$dir->$url/v${version.value}/"
      }
  }
)

val commonNativeSettings = commonSettings ++ Seq(
  scalaVersion := scala2_11,
  crossScalaVersions := Seq(scala2_11),
  nativeLinkStubs := true
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

val scalaTestVersion = "3.2.0"
val scalaNativeTestInterfaceVersion = "0.4.0-M2"

lazy val rootProjectAggregates: Seq[ProjectReference] = if (sys.env.isDefinedAt("STTP_NATIVE")) {
  println("[info] STTP_NATIVE defined, including sttp-native in the aggregate projects")
  List(rootJVM, rootJS, rootNative)
} else {
  println("[info] STTP_NATIVE *not* defined, *not* including sttp-native in the aggregate projects")
  List(rootJVM, rootJS)
}

val compileAndTest = "compile->compile;test->test"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  // setting version to 2.11 so that cross-releasing works. Don't ask why.
  .settings(skip in publish := true, name := "sttp-model", scalaVersion := scala2_11, crossScalaVersions := Seq())
  .aggregate(rootProjectAggregates: _*)

lazy val rootJVM = project
  .in(file(".jvm"))
  .settings(commonJvmJsSettings: _*)
  .settings(skip in publish := true, name := "sttp-model-jvm")
  .aggregate(coreJVM)

lazy val rootJS = project
  .in(file(".js"))
  .settings(commonJvmJsSettings: _*)
  .settings(skip in publish := true, name := "sttp-model-js")
  .aggregate(coreJS)

lazy val rootNative = project
  .in(file(".native"))
  .settings(commonNativeSettings: _*)
  .settings(skip in publish := true, name := "sttp-model-native")
  .aggregate(coreNative)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(name := "core")
  .jvmSettings(commonJvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .nativeSettings(commonNativeSettings: _*)
  .nativeSettings(only2_11settings)
  .jvmSettings(libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalaTestVersion % Test))
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.0.0",
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
    )
  )
  .jsSettings(browserTestSettings)
  .nativeSettings(
    libraryDependencies ++= Seq(
      "org.scala-native" %%% "test-interface" % scalaNativeTestInterfaceVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
    )
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val coreNative = core.native
