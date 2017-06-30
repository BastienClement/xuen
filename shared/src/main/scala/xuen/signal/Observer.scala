package xuen.signal

import scala.util.{Failure, Success, Try}
import xuen.signal.tools.TracingContext

final class Observer private (body: => Unit) {
	private var bound = false
	private var parents: List[Mutable[_]] = Nil

	private[signal] def trigger(): Unit = {
		parents.foreach(_ detach this)
		val (ret, ps) = TracingContext.trace(Try(body))
		parents = ps
		parents.foreach(_ attach this)
		ret match {
			case Success(_) => ()
			case Failure(e) => throw e
		}
	}

	def bind(): this.type = {
		if (!bound) {
			bound = true
			trigger()
		}
		this
	}

	def unbind(): this.type = {
		if (bound) {
			bound = false
			parents.foreach(_ detach this)
			parents = Nil
		}
		this
	}
}

object Observer {
	def apply(body: => Unit): Observer = new Observer(body).bind()
}
