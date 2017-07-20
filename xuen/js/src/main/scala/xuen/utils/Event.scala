package xuen.utils

import org.scalajs.dom
import scala.scalajs.js

object Event {
	import js.Dynamic.{global => g, literal => lit, newInstance => jsnew}

	def createCustom(name: String, detail: Any = null, composed: Boolean = false,
	                 bubbles: Boolean = false, cancelable: Boolean = false): dom.CustomEvent = {
		jsnew(g.CustomEvent)(name, lit(
			detail = detail.asInstanceOf[js.Any],
			composed = composed,
			bubbles = bubbles,
			cancelable = cancelable
		)).asInstanceOf[dom.CustomEvent]
	}

	def dispatchCustom(target: dom.EventTarget, name: String, detail: Any = null, composed: Boolean = false,
	                   bubbles: Boolean = false, cancelable: Boolean = false): Boolean = {
		dom.document.dispatchEvent(createCustom(name, detail,
			composed = composed,
			bubbles = bubbles,
			cancelable = cancelable))
	}

	implicit final class CustomDispatch (private val target: dom.EventTarget) extends AnyVal {
		@inline def dispatchEvent(name: String, detail: Any = null, composed: Boolean = false,
		                          bubbles: Boolean = false, cancelable: Boolean = false): Boolean = {
			dispatchCustom(target, name, detail,
				composed = composed,
				bubbles = bubbles,
				cancelable = cancelable)
		}
	}
}
