libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.1"
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.15.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.17")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.10.0")

val sbtSoftwareMillVersion = "2.0.20"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-browser-test-js" % sbtSoftwareMillVersion)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
