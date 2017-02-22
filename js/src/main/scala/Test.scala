import scala.concurrent.Future
import scala.scalajs.js.JSApp
import xuen.signal.Signal
import scala.concurrent.ExecutionContext.Implicits.global

object Test extends JSApp {
	@scala.scalajs.js.annotation.JSExport
	def main(): Unit = {
		val test = Signal.wrap(Some(2))
		val test2: Signal[Int] = Future.successful(2)
		val test3: Signal[Int] = 3

		println(test, test2, test3)
	}
}
