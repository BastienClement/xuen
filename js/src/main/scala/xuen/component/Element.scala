package xuen.component

import org.scalajs.dom.html
import scala.scalajs.js.annotation.ScalaJSDefined
import xuen.facades.webcomponents.ShadowDOM.enableShadowDOM
import xuen.facades.webcomponents.{ShadowRoot, ShadowRootInit, ShadowRootMode}
import xuen.signal.Signal

@ScalaJSDefined
abstract class Element(val component: Component) extends html.Element {
	val shadow: ShadowRoot = this.attachShadow(new ShadowRootInit {
		val mode: ShadowRootMode = ShadowRootMode.open
	})

	def attribute[T](implicit enclosing: sourcecode.Enclosing): Signal[T] = {
		".+#([a-zA-Z]+)$".r.findFirstMatchIn(enclosing.value).map(_.group(1)) match {
			case Some(camelCase) =>
				val hyphenated = camelCase.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
				attribute(hyphenated)
			case None =>
				throw new Error(s"Unable to derive attribute name from `${enclosing.value}`. " +
				                "Attribute name for this binding must be explicitly defined.")
		}
	}

	def attribute[T](name: String): Signal[T] = {
		println(name)
		Signal.undefined
	}

	for (styles <- component.optionalStylesheet) shadow.appendChild(styles.node.cloneNode(true))
	for (tpl <- component.optionalTemplate) shadow.appendChild(tpl.node.content.cloneNode(true))
}
