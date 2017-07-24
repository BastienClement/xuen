package xuen.component

import org.scalajs.dom
import org.scalajs.dom.{html, MutationObserver}
import org.scalajs.dom.raw.MutationObserverInit
import scala.language.implicitConversions
import scala.scalajs.js
import scala.util.DynamicVariable
import xuen.component.Element.AttributeBinding
import xuen.expression.Context
import xuen.facades.webcomponents.{ShadowRoot, ShadowRootInit, ShadowRootMode}
import xuen.facades.webcomponents.ShadowDOM.enableShadowDOM
import xuen.signal.{Signal, Source}
import xuen.template.TemplateInstance
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
	/** The shadow root of this element. */
	def shadow: ShadowRoot = this.shadowRoot.filter(_ != null) getOrElse this.attachShadow(new ShadowRootInit {
		val mode: ShadowRootMode = ShadowRootMode.open
	})

	/** Implicit reference to this element */
	protected implicit val self: Element = this

	/** Defined attribute bindings for this element */
	private var attributeBindings = Map.empty[String, AttributeBinding[_]]

	// Instantiate template
	val template = for (tpl <- component.template) yield {
		val instance = tpl.instantiate(new Context.Root(this.asInstanceOf[js.Dynamic], shadow.querySelector _))
		// Work-around for weird upgrade timing with custom elements
		Element.upgradeAndMount(instance, shadow)
		instance
	}

	// Append stylesheet
	val stylesheet = for (sheet <- component.stylesheet) yield {
		val styles = sheet.node.cloneNode(true).asInstanceOf[html.Style]
		shadow.appendChild(styles)
		styles
	}

	/** The connected state of the component */
	def isConnected: Boolean = connected
	private var connected = false

	/** Called by the browser when the element is connected to the document DOM */
	final def connectedCallback(): Unit = if (!Element.forcedUpgrade.value) {
		connected = true
		for (tpl <- template) tpl.enable()
		this.dispatchEvent("xuen:connected")
	}

	/** Called by the browser when the element is disconnected from the document DOM */
	final def disconnectedCallback(): Unit = if (!Element.forcedUpgrade.value) {
		connected = false
		dispatchEvent("xuen:disconnected")
		for (tpl <- template) tpl.disable()
	}

	/**
	  * Setups a attribute binding with the given type.
	  *
	  * The name of the attribute is automatically identified based on the
	  * name of the value being defined by this call. As an example, the code:
	  * `val foo = attribute[String]`
	  * would create an binding for the "foo" attribute.
	  *
	  * @param enclosing the enclosing value definition, filled in by the compiler
	  * @tparam T the type of value of this attribute
	  * @return a signal of the value of this attribute
	  */
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

	/** Setup an attribute binding with the given name and type */
	def attribute[T](name: String)(implicit fmt: AttributeFormat[T]): Source[T] = {
		require(!attributeBindings.contains(name), "a binding is already defined for this attribute")
		val binding = AttributeBinding[T](this, name)
		attributeBindings = attributeBindings.updated(name, binding)
		if (attributeBindings.size == 1) setupAttributeObserver()
		binding.signal
	}

	/** Setups an undefined property of the given type */
	def property[T](implicit dummy: DummyImplicit): Source[T] = Source.undefined

	/** Setups a property with the given initial value */
	def property[T](value: T): Source[T] = Source(value)

	/** Listen to a custom event */
	def listenEvent[T <: dom.Event](event: Event[T], flags: Event.ListenerFlags = Event.ListenerFlagsDefault)
	                               (handler: T => Unit): Event.Listener = {
		Event.listenCustom(this, event, flags, handler)
	}

	/** Dispatches a custom event */
	def dispatchEvent(name: String, detail: Any = null, flags: Event.EventFlags = Event.EventFlagsDefault): Boolean = {
		Event.dispatchCustom(this, name, detail, flags)
	}

	/** Setups the mutation observer used to drive attribute bindings */
	private def setupAttributeObserver(): Unit = {
		new MutationObserver((records, _) => {
			for {
				record <- records
				name = record.attributeName
				binding <- attributeBindings.get(name)
				newValue = if (hasAttribute(name)) Some(getAttribute(name)) else None
			} binding.update(newValue)
		}).observe(this, MutationObserverInit(attributes = true))
	}
}

object Element {
	private val forcedUpgradeContainer = dom.document.createElement("xuen:upgrade")
	private val forcedUpgrade = new DynamicVariable[Boolean](false)

	/** Force upgrade of custom element in the template and then mount it */
	private def upgradeAndMount(instance: TemplateInstance, host: dom.Node, before: dom.Element = null): Unit = {
		val first = !forcedUpgrade.value
		if (first) dom.document.documentElement.appendChild(forcedUpgradeContainer)
		forcedUpgrade.withValue(true) {
			instance.mount(forcedUpgradeContainer)
			instance.mount(host, before)
		}
		if (first) dom.document.documentElement.removeChild(forcedUpgradeContainer)
	}

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
