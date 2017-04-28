package xuen.facades.webcomponents

import scala.scalajs.js

@js.native
sealed trait ShadowRootMode extends scalajs.js.Any

object ShadowRootMode {
	val open: ShadowRootMode = "open".asInstanceOf[ShadowRootMode]
	val closed: ShadowRootMode = "closed".asInstanceOf[ShadowRootMode]
}
