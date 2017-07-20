package xuen.template

import org.scalajs.dom
import org.scalajs.dom.ext.PimpedNodeList
import xuen.expression.Context
import xuen.facades.webcomponents.HTMLTemplateElement
import xuen.signal.Signal

case class TemplateInstance private[template] (template: Template, root: HTMLTemplateElement, context: Context) {
	/** The current enable state of the template */
	private var state: Boolean = false

	/** The set of attached child template */
	private var attached: Set[TemplateInstance] = Set.empty

	// Instantiate behaviors and collect child signals and observers
	private val signalGroup = Signal.grouped {
		for ((id, behavior) <- template.behaviors) {
			root.content.querySelector( s"""[xuen\\:behavior="$id"]""") match {
				case null => throw new IllegalStateException(s"Unable to find element for behavior '$id'")
				case element =>
					val node = behavior.build(element, this)
					if (node != element) element.parentNode.replaceChild(node, element)
			}
		}
	}

	/** The list of top-level nodes in this template */
	val nodes = root.content.childNodes.toVector

	def enabled: Boolean = state
	def disabled: Boolean = !state

	def enable(): Unit = if (disabled) {
		state = true
		signalGroup.bindObservers()
		for (child <- attached) child.enable()
	}

	def disable(): Unit = if (enabled) {
		for (child <- attached) child.disable()
		signalGroup.unbindObservers()
		signalGroup.invalidateSignals(disconnect = true)
		state = false
	}

	def attach(child: TemplateInstance): Unit = {
		attached += child
		if (enabled) child.enable()
	}

	def detach(child: TemplateInstance): Unit = {
		attached -= child
		child.disable()
	}

	def mount(host: dom.Node, before: dom.Node = null): Unit = {
		nodes.fold(before)((b, n) => {
			host.insertBefore(n, b)
			n.nextSibling
		})
	}

	def unmount(): Unit = mount(root.content)
}
