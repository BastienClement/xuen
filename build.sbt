import org.openqa.selenium.chrome.ChromeOptions
import org.scalajs.core.tools.linker.backend.OutputMode.ECMAScript6
import org.scalajs.jsenv.selenium.SeleniumJSEnv
import sbtcrossproject.{crossProject, CrossType}

val ChromeEnv = {
	val opts = new ChromeOptions
	opts.addArguments("--headless")
	org.scalajs.jsenv.selenium.Chrome().withChromeOptions(opts)
}

// Disable root project publishing
publishLocal := {}
publish := {}

lazy val xuen = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full).settings(
	name := "xuen",
	organization := "me.galedric",
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
	libraryDependencies ++= Seq(
		"org.scalatest" %%% "scalatest" % "3.0.2" % "test"
	)
).jvmSettings(
).jsSettings(
	jsEnv := new SeleniumJSEnv(ChromeEnv),
	//scalaJSUseMainModuleInitializer := true,
	skip in packageJSDependencies := false,
	scalaJSOutputMode := ECMAScript6,
	scalacOptions ++= Seq(
		"-language:reflectiveCalls",
		"-P:scalajs:sjsDefinedByDefault"
	),
	libraryDependencies ++= Seq(
		"org.scala-js" %%% "scalajs-dom" % "0.9.3",
		"com.lihaoyi" %%% "sourcecode" % "0.1.3",
		"org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"
	)
) //.jsConfigure(_.enablePlugins(JSDependenciesPlugin))

lazy val xuenJVM = xuen.jvm
lazy val xuenJS = xuen.js.enablePlugins(ScalaJSPlugin)

def chromeDriver = if (System.getProperty("os.name").startsWith("Windows")) "chromedriver.exe" else "chromedriver"
javaOptions += "-Dwebdriver.chrome.driver=" + (baseDirectory.value / chromeDriver).getAbsolutePath

logBuffered in Test := false
