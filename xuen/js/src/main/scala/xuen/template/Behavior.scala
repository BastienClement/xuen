package xuen.template

import org.scalajs.dom
import scala.collection.mutable.ListBuffer

case class Behavior[N <: dom.Node] (adapter: dom.Element => N) {
	val builders = ListBuffer.empty[Behavior.Builder[N]]

	def build(element: dom.Element, instance: TemplateInstance): dom.Node = {
		val node = adapter(element)
		for (builder <- builders) builder(node, instance)
		node
	}
}

object Behavior {
	type Builder[N <: dom.Node] = (N, TemplateInstance) => Unit
}
