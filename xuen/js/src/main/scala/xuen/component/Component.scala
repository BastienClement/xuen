package xuen.component

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.ConstructorTag
import xuen.template.Template

/**
  * A Xuen component definition.
  *
  * An instance of this class describes fundamentals properties of a Xuen component
  * such as its selector, its element implementation, its template and stylesheet.
  *
  * @param selector     the component selector (must contain a dash)
  * @param dependencies a list of components on which this component is dependant
  * @param template     the component template
  * @param stylesheet   the component stylesheet
  */
abstract class Component[E <: Element : ConstructorTag](val selector: String,
                                                        val dependencies: Seq[Component[_]] = Seq.empty,
                                                        val template: js.UndefOr[Template] = js.undefined,
                                                        val stylesheet: js.UndefOr[Stylesheet] = js.undefined) {
	// Registers this component as a custom element
	js.Dynamic.global.customElements.define(selector, implicitly[ConstructorTag[E]].constructor)

	// Dependencies check
	for (tpl <- template) {
		val declared = dependencies.map(_.selector).toSet
		val detected = tpl.detectedDependencies

		// Self selector is included in declared dependencies if a recursive component is constructed
		val missing = detected diff (declared + selector)
		if (missing.nonEmpty) {
			val list = missing.mkString("<", ">, <", ">")
			dom.console.warn(s"Missing component dependencies: <$selector> => $list")
		}

		val unused = declared diff detected
		if (unused.nonEmpty) {
			val list = unused.mkString("<", ">, <", ">")
			dom.console.warn(s"Unused component dependencies: <$selector> => $list")
		}
	}

	def instantiate(): E = dom.document.createElement(selector).asInstanceOf[E]
}
