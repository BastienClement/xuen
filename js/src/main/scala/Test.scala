import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.ScalaJSDefined
import xuen.component.{Component, Element}
import xuen.expression.Expression

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
		println(Expression.parse(""" a = 2 """))
	}
}
