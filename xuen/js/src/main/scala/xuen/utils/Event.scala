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

	case class EventFlags (composed: js.UndefOr[Boolean] = js.undefined,
	                       bubbles: js.UndefOr[Boolean] = js.undefined,
	                       cancelable: js.UndefOr[Boolean] = js.undefined)
	val EventFlagsDefault = EventFlags()

	def createCustom(name: String, detail: Any = null, flags: EventFlags = EventFlagsDefault): dom.CustomEvent = {
		jsnew(g.CustomEvent)(name, lit(
			detail = detail.asInstanceOf[js.Any],
			composed = flags.composed,
			bubbles = flags.bubbles,
			cancelable = flags.cancelable
		)).asInstanceOf[dom.CustomEvent]
	}

	def dispatchCustom(target: dom.EventTarget, name: String, detail: Any = null,
	                   flags: EventFlags = EventFlagsDefault): Boolean = {
		target.dispatchEvent(createCustom(name, detail, flags))
	}

	case class ListenerFlags (capture: js.UndefOr[Boolean] = js.undefined,
	                          passive: js.UndefOr[Boolean] = js.undefined,
	                          once: js.UndefOr[Boolean] = js.undefined)
	val ListenerFlagsDefault = ListenerFlags()

	case class Listener (target: dom.EventTarget, event: String, fn: js.Function1[_ <: dom.Event, _], flags: ListenerFlags) {
		def remove(): Unit = target.asInstanceOf[js.Dynamic].removeEventListener(event, fn, lit(
			capture = flags.capture,
			passive = flags.passive
		))
	}

	def listenCustom[T <: dom.Event](target: dom.EventTarget, event: Event[T],
	                                 flags: ListenerFlags = ListenerFlagsDefault,
	                                 handler: T => Unit): Listener = {
		val fn: js.Function1[_ <: dom.Event, Unit] = handler
		target.asInstanceOf[js.Dynamic].addEventListener(event.name, fn, lit(
			capture = flags.capture,
			passive = flags.passive,
			once = flags.once
		))
		Listener(target, event.name, fn, flags)
	}

	implicit final class CustomListen(private val target: dom.EventTarget) extends AnyVal {
		@inline def listenEvent[T <: dom.Event](event: Event[T], flags: ListenerFlags = ListenerFlagsDefault)
		                                       (handler: T => Unit): Listener = {
			listenCustom(target, event, flags, handler)
		}
	}

	implicit final class CustomDispatch (private val target: dom.EventTarget) extends AnyVal {
		@inline def dispatchEvent(name: String, detail: Any = null, flags: EventFlags = EventFlagsDefault): Boolean = {
			dispatchCustom(target, name, detail, flags)
		}
	}
}
