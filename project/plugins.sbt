libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.1"
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.2")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.7")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.10.1")

val sbtSoftwareMillVersion = "2.0.25"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-browser-test-js" % sbtSoftwareMillVersion)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
