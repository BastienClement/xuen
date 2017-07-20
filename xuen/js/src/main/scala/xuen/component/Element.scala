package xuen.component

import org.scalajs.dom.{html, MutationObserver}
import org.scalajs.dom.raw.MutationObserverInit
import scala.scalajs.js
import xuen.component.Element.AttributeBinding
import xuen.expression.Context
import xuen.facades.webcomponents.{ShadowRoot, ShadowRootInit, ShadowRootMode}
import xuen.facades.webcomponents.ShadowDOM.enableShadowDOM
import xuen.signal.{Signal, Source}
import xuen.utils.Event

/**
  * A Xuen element implementation.
  *
  * An instance of this class represents an instance of the associated
  * Xuen component.
  *
  * @param component the component that this element is implementing
  */
abstract class Element (component: Component[_]) extends html.Element {
	/** The shadow root of this element */
	val shadow: ShadowRoot = this.attachShadow(new ShadowRootInit {
		val mode: ShadowRootMode = ShadowRootMode.open
	})

	/** Defined attribute bindings for this element */
	private var attributeBindings = Map.empty[String, AttributeBinding[_]]

	// Instantiate template
	val template = for (tpl <- component.template) yield {
		val instance = tpl.instantiate(new Context.Root(this.asInstanceOf[js.Dynamic], this.shadow.querySelector _))
		instance.mount(shadow)
		instance
	}

	// Append stylesheet
	val stylesheet = for (sheet <- component.stylesheet) {
		val styles = sheet.node.cloneNode(true)
		shadow.appendChild(styles)
		styles
	}

	def connected(): Unit = ()
	def disconnected(): Unit = ()

	final def connectedCallback(): Unit = {
		for (tpl <- template) tpl.enable()
		connected()
	}

	final def disconnectedCallback(): Unit = {
		disconnected()
		for (tpl <- template) tpl.disable()
	}

	def attribute[T: AttributeFormat](implicit enclosing: sourcecode.Enclosing): Source[T] = {
		".+#([a-zA-Z]+)$".r.findFirstMatchIn(enclosing.value).map(_.group(1)) match {
			case Some(camelCase) =>
				val hyphenated = camelCase.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
				attribute(hyphenated)
			case None =>
				throw new Error(s"Unable to derive attribute name from `${enclosing.value}`. " +
				                "Attribute name for this binding must be explicitly defined.")
		}
	}

	def attribute[T](name: String)(implicit fmt: AttributeFormat[T]): Source[T] = {
		require(!attributeBindings.contains(name), "a binding is already defined for this attribute")
		val binding = AttributeBinding[T](this, name)
		attributeBindings = attributeBindings.updated(name, binding)
		if (attributeBindings.size == 1) {
			new MutationObserver((records, _) => {
				for {
					record <- records
					name = record.attributeName
					binding <- attributeBindings.get(name)
					newValue = if (hasAttribute(name)) Some(getAttribute(name)) else None
				} binding.update(newValue)
			}).observe(this, MutationObserverInit(attributes = true))
		}
		binding.signal
	}

	def property[T](implicit dummy: DummyImplicit): Source[T] = Source.undefined
	def property[T](value: T): Source[T] = Source(value)

	def dispatchEvent(name: String, detail: Any = null, composed: Boolean = false,
	         bubbles: Boolean = false, cancelable: Boolean = false): Boolean = {
		Event.dispatchCustom(this, name, detail,
			composed = composed,
			bubbles = bubbles,
			cancelable = cancelable)
	}
}

object Element {
	case class AttributeBinding[T](el: Element, name: String)(implicit fmt: AttributeFormat[T]) {
		/** The current string value of the attribute */
		private var current: Option[String] = if (el.hasAttribute(name)) Some(el.getAttribute(name)) else None

		val signal = current match {
			case None => Source.undefined[T]
			case Some(value) => Source(fmt.decode(value))
		}

		val observer = for (newValue <- signal.map(fmt.encode).wrap if current != newValue) {
			current = newValue
			newValue match {
				case None => el.removeAttribute(name)
				case Some(value) => el.setAttribute(name, value)
			}
		}

		def update(newValue: Option[String]): Unit = if (current != newValue) {
			current = newValue
			newValue match {
				case None => signal := Signal.nil
				case Some(value) => signal := fmt.decode(value)
			}
		}
	}
}
