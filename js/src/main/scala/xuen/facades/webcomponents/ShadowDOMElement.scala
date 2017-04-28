package xuen.facades.webcomponents

import scala.scalajs.js

@js.native
trait ShadowDOMElement extends js.Any {
	def attachShadow(init: ShadowRootInit): ShadowRoot = js.native
	def shadowRoot: js.UndefOr[ShadowRoot] = js.native
}
