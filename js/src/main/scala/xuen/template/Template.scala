package xuen.template

import org.scalajs.dom
import xuen.expression.Context
import xuen.facades.webcomponents.HTMLTemplateElement

case class Template(node: HTMLTemplateElement, name: String = "?") {
	private val res = Compiler.compile(node, name)

	def instantiate(context: Context) = TemplateInstance(this, context)
}

object Template {
	private def createTemplateElement(source: String): HTMLTemplateElement = {
		val template = dom.document.createElement("template").asInstanceOf[HTMLTemplateElement]
		template.innerHTML = source
		template
	}

	def interpolate(sc: StringContext, args: Seq[Any]): Template = {
		require(sc.parts.length == 1 && args.isEmpty, "template interpolator does not support substitutions")
		Template(createTemplateElement(sc.parts.head))
	}

	implicit final class Interpolation(private val sc: StringContext) extends AnyVal {
		@inline def html(args: Any*): Template = interpolate(sc, args)
	}
}
