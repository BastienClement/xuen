logLevel := Level.Warn

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.15")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")

libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "0.1.3"
