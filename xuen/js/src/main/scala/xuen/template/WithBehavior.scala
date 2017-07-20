package xuen.template

import org.scalajs.dom

/**
  * A type-class for types of objects that can be associated with a behavior
  * inside a Xuen template.
  *
  * @tparam T the type of the object
  * @tparam N the type of node at run-time
  */
trait WithBehavior[T, N <: dom.Node] {
	def getBehavior(target: T)(implicit tpl: Template): Behavior[N]
}

object WithBehavior {
	object PlaceholderBehavior extends WithBehavior[Placeholder[dom.Node], dom.Node] {
		@inline def getBehavior(target: Placeholder[dom.Node])
		                       (implicit tpl: Template): Behavior[dom.Node] = {
			tpl.behaviors.get(target.asInstanceOf[dom.Element].getAttribute("xuen:behavior")) match {
				case Some(behavior) => behavior.asInstanceOf[Behavior[dom.Node]]
				case None => throw new IllegalStateException("No behavior associated with placeholder element")
			}
		}
	}

	@inline implicit def placeholder[N <: dom.Node]: WithBehavior[Placeholder[N], N] =
		PlaceholderBehavior.asInstanceOf[WithBehavior[Placeholder[N], N]]

	private def removeBehaviorAttribute(element: dom.Element): dom.Element = {
		element.removeAttribute("xuen:behavior")
		element
	}

	object ElementBehavior extends WithBehavior[dom.Element, dom.Element] {
		@inline def getBehavior(target: dom.Element)(implicit tpl: Template): Behavior[dom.Element] = {
			if (!target.hasAttribute("xuen:behavior")) {
				val id = Compiler.nextBehaviorId
				target.setAttribute("xuen:behavior", id)
				tpl.behaviors += (id -> Behavior(removeBehaviorAttribute))
			}
			tpl.behaviors.get(target.getAttribute("xuen:behavior")) match {
				case Some(behavior) => behavior.asInstanceOf[Behavior[dom.Element]]
				case None => throw new IllegalStateException("No behavior associated with tagged element")
			}
		}
	}

	@inline implicit def element[E <: dom.Element]: WithBehavior[E, E] =
		ElementBehavior.asInstanceOf[WithBehavior[E, E]]
}
