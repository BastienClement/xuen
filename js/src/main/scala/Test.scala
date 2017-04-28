import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.ScalaJSDefined
import xuen.component.{Component, Element}

object FooBar extends Component(selector = "foo-bar", implementation = js.constructorOf[FooBar])
		with Component.Template with Component.Stylesheet {

	val template = html"""
		Hello world !
		<!-- I'm a comment baby! -->
	"""

	val stylesheet = less"""
		:host {
			color: var(--text-color);
		}
	"""
}

@ScalaJSDefined
class FooBar extends Element(FooBar) {
	def connectedCallback(): Unit = {
		println("Element is connected")
	}

	val withFooBar = attribute[String]
}

object Test extends JSApp {
	@scala.scalajs.js.annotation.JSExport
	def main(): Unit = {
		dom.console.log(dom.document.asInstanceOf[js.Dynamic].currentScript)
		val e = new FooBar
		dom.document.body.appendChild(e)
		val f = new FooBar
		dom.document.body.appendChild(f)
	}
}
