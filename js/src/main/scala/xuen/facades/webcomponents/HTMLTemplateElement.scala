package xuen.facades.webcomponents

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal
abstract class HTMLTemplateElement extends html.Element {
	def content: dom.DocumentFragment = js.native
}
