import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.ScalaJSDefined
import xuen.component.{Component, Element}
import xuen.expression.{Context, Expression, Interpreter}

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
		val e = Expression.parse(""" { a: printThis }?.a.b() """)
		println(e)
		val v = Interpreter.evaluate(e.right.get)(new Context.Reference(dom.window.asInstanceOf[js.Dynamic]))
		println(v)
	}
}
