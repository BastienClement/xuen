package xuen.facades.webcomponents

import org.scalajs.dom
import scala.language.implicitConversions

object ShadowDOM {
	implicit def enableShadowDOM(e: dom.Element): ShadowDOMElement = e.asInstanceOf[ShadowDOMElement]
}
