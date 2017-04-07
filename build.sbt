
lazy val root = project.in(file(".")).aggregate(xuenJvm, xuenJs).settings(
	publish := {},
	publishLocal := {}
)

lazy val xuen = crossProject.in(file(".")).settings(
	name := "xuen" ,
	version := "1.0-SNAPSHOT",
	scalaVersion := "2.12.1",
	scalacOptions ++= Seq(
		//"-Xlog-implicits",
		"-feature",
		"-deprecation",
		"-Xfatal-warnings",
		"-unchecked",
		"-language:reflectiveCalls",
		"-language:higherKinds"
	),
	crossPaths := false,
	libraryDependencies ++= Seq(
		"org.scalatest" %%% "scalatest" % "3.0.1" % "test"
	)
).jvmSettings(
).jsSettings(
)

lazy val xuenJvm = xuen.jvm
lazy val xuenJs = xuen.js.enablePlugins(ScalaJSPlugin)
