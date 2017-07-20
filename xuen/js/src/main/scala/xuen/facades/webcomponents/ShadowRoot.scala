package xuen.facades.webcomponents

import org.scalajs.dom
import scala.scalajs.js

@js.native
trait ShadowRoot extends dom.DocumentFragment {
	def mode: ShadowRootMode = js.native
	def host: dom.Element = js.native
}
