logLevel := Level.Warn

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.18")
//addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.0-M1")
addSbtPlugin("org.scala-native" % "sbt-crossproject" % "0.2.0")
addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")

libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "0.1.3"
