import scala.scalajs.js.JSApp
import xuen.signal.Signal

object Test extends JSApp {
	@scala.scalajs.js.annotation.JSExport
	def main(): Unit = {
		val test = Signal.wrap(Some(2))
		val test3: Signal[Int] = 3

		println(test, test3)
	}
}
