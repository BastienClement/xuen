package xuen.template

import org.scalajs.dom
import scala.collection.mutable.ListBuffer
import xuen.expression.Context
import xuen.facades.webcomponents.HTMLTemplateElement

/**
  * A Xuen template object. Such template are created by the template compiler
  * by invoking the [[Compiler.compile]] method with the original element.
  *
  * Templates defines a set of _behaviors_ for nodes in the template that
  * should respond to change in the given template context and thus form the
  * basis of bindings between signals in the component implementation and their
  * visual rendering as DOM nodes in the template.
  *
  * @param root   the root template element
  * @param parent an optional parent template
  */
case class Template private[template] (root: HTMLTemplateElement, parent: Option[Template] = None) {
	private[template] var behaviors: Map[String, Behavior[_]] = Map.empty
	private[template] var dependencies: Set[String] = Set.empty
	private[template] val children: ListBuffer[Template] = ListBuffer.empty

	// Add self to parent if given
	for (p <- parent) p.children += this

	/** The set of detected dependencies from this template and every children */
	def detectedDependencies: Set[String] = children.map(_.detectedDependencies).fold(dependencies)(_ ++ _)

	/**
	  * Instantiates this template.
	  *
	  * @param context the context used by the instance
	  * @return
	  */
	def instantiate(context: Context): TemplateInstance = {
		TemplateInstance(this, root.cloneNode(true).asInstanceOf[HTMLTemplateElement], context)
	}
}

object Template {
	def apply(source: String): Template = {
		val template = dom.document.createElement("template").asInstanceOf[HTMLTemplateElement]
		template.innerHTML = source
		Compiler.compile(template)
	}

	def interpolate(sc: StringContext, args: Seq[Any]): Template = {
		require(sc.parts.length == 1 && args.isEmpty, "template interpolator does not support substitutions")
		apply(sc.parts.head)
	}
}
