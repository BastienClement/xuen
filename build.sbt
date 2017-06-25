name := "Xuen"

normalizedName := "xuen"

lazy val xuen = crossProject.in(file(".")).settings(
	version := "1.0-SNAPSHOT",
	scalaVersion := "2.12.2",
	scalacOptions ++= Seq(
		//"-Xlog-implicits",
		"-feature",
		"-deprecation",
		"-Xfatal-warnings",
		"-unchecked",
		"-language:higherKinds"
	),
	crossPaths := false,
	libraryDependencies ++= Seq(
		"org.scalatest" %%% "scalatest" % "3.0.2" % "test"
	)
).jvmSettings(
).jsSettings(
	jsEnv := new org.scalajs.jsenv.selenium.SeleniumJSEnv(org.scalajs.jsenv.selenium.Chrome()),
	scalaJSOutputMode := org.scalajs.core.tools.linker.backend.OutputMode.ECMAScript6,
	scalaJSUseMainModuleInitializer := true,
	skip in packageJSDependencies := false,
	scalacOptions ++= Seq(
		"-language:reflectiveCalls"
	),
	libraryDependencies ++= Seq(
		"org.scala-js" %%% "scalajs-dom" % "0.9.1",
		"com.lihaoyi" %%% "sourcecode" % "0.1.3",
		"org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"
	),
	jsDependencies ++= Seq(
		"org.webjars.bower" % "less" % "2.7.2" /
			"2.7.2/dist/less.js" minified "2.7.2/dist/less.min.js"
	)
)

lazy val xuenJvm = xuen.jvm
lazy val xuenJs = xuen.js.enablePlugins(ScalaJSPlugin)

def chromeDriver = if (System.getProperty("os.name").startsWith("Windows")) "chromedriver.exe" else "chromedriver"
javaOptions += "-Dwebdriver.chrome.driver=" + (baseDirectory.value / chromeDriver).getAbsolutePath
