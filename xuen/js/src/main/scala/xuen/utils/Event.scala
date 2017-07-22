package xuen.utils

import org.scalajs.dom
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g, literal => lit, newInstance => jsnew}

final class Event[T <: dom.Event](val name: String) extends AnyVal

object Event {
	val Blur = new Event[dom.FocusEvent]("blur")
	val Click = new Event[dom.MouseEvent]("click")
	val Close = new Event[dom.CloseEvent]("close")
	val Error = new Event[dom.Event]("error")
	val Input = new Event[dom.Event]("input")
	val KeyDown = new Event[dom.KeyboardEvent]("keydown")
	val KeyUp = new Event[dom.KeyboardEvent]("keyup")
	val Load = new Event[dom.UIEvent]("load")
	val Message = new Event[dom.MessageEvent]("message")
	val Open = new Event[dom.Event]("open")
	val Resize = new Event[dom.UIEvent]("resize")
	val TransitionEnd = new Event[dom.TransitionEvent]("transitionend")

	@inline
	implicit def Custom(name: String): Event[dom.CustomEvent] = new Event[dom.CustomEvent](name)

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

	case class Listener (target: dom.EventTarget, event: String, fn: js.Function1[_ <: dom.Event, _], capture: Boolean) {
		def remove(): Unit = target.removeEventListener(event, fn, capture)
	}

	implicit final class CustomRegister(private val target: dom.EventTarget) extends AnyVal {
		@inline def on[T <: dom.Event](event: Event[T], capture: Boolean = false)(handler: T => Unit): Listener = {
			val fn: js.Function1[_ <: dom.Event, Unit] = handler
			target.addEventListener(event.name, fn, capture)
			Listener(target, event.name, fn, capture)
		}
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
