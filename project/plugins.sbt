libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.0"
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.3.1")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.0-M2")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.6.0")

val sbtSoftwareMillVersion = "1.9.14"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.6")

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")