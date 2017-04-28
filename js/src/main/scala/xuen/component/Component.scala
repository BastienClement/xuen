package xuen.component

import scala.language.{dynamics, implicitConversions}
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import xuen.component.{Stylesheet => XStylesheet}
import xuen.template.{Template => XTemplate}

abstract class Component(val selector: String,
                         val implementation: js.UndefOr[js.Dynamic] = js.undefined,
                         val dependencies: Seq[Component] = Seq.empty) {

	js.Dynamic.global.customElements.define(selector, implementation orElse new Element(this) {})

	def optionalTemplate: js.UndefOr[XTemplate] = js.undefined
	def optionalStylesheet: js.UndefOr[XStylesheet] = js.undefined
}

object Component {
	trait Template extends Component {
		val template: XTemplate
		override def optionalTemplate: UndefOr[XTemplate] = template
		@inline protected implicit final def htmlInterpolation(sc: StringContext): XTemplate.Interpolation = {
			new XTemplate.Interpolation(sc)
		}
	}

	trait Stylesheet extends Component {
		val stylesheet: XStylesheet
		override def optionalStylesheet: UndefOr[XStylesheet] = stylesheet
		@inline protected implicit final def lessInterpolation(sc: StringContext): XStylesheet.Interpolation = {
			new XStylesheet.Interpolation(sc)
		}
	}
}
