package xuen.component

import org.scalajs.dom
import org.scalajs.dom.html

case class Stylesheet (node: html.Style) {
	//dom.console.log(node)
}

object Stylesheet {
	def apply(source: String): Stylesheet = {
		val node = dom.document.createElement("style").asInstanceOf[html.Style]
		node.textContent = source
		Stylesheet(node)
	}

	def interpolate(sc: StringContext, args: Seq[Any]): Stylesheet = {
		require(sc.parts.length == 1 && args.isEmpty, "stylesheet interpolator does not support substitutions")
		apply(sc.parts.head)
	}
}
