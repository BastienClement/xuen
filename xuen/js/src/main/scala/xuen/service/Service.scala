package xuen.service

import org.scalajs.dom
import scala.concurrent.duration._
import scala.scalajs.js.timers._
import xuen.component.Element

abstract class Service (timeout: FiniteDuration = 5.second) {
	private var acquired: Int = 0
	private var enabled = false
	private var timeoutTimer: SetTimeoutHandle = null

	protected def enable(): Unit
	protected def disable(): Unit

	final def acquire(): this.type = {
		acquired += 1
		if (acquired >= 1 && !enabled) {
			enabled = true
			clearTimeout(timeoutTimer)
			timeoutTimer = null
			enable()
		}
		this
	}

	final def release(): this.type = {
		acquired = (acquired - 1) max 0
		if (acquired < 1 && enabled) {
			timeoutTimer = setTimeout(timeout) {
				if (enabled) {
					enabled = false
					timeoutTimer = null
					disable()
				}
			}
		}
		this
	}

	final def isEnabled: Boolean = enabled
}

object Service {
	def use[S <: Service](service: S)(implicit element: Element): S = {
		var acquired = false
		def connected(e: dom.Event): Unit = {
			if (acquired) throw new IllegalStateException("Service acquired more than one time")
			acquired = true
			service.acquire()
		}
		def disconnected(e: dom.Event): Unit = {
			if (!acquired) throw new IllegalStateException("Service released more than one time")
			acquired = false
			service.release()
		}
		element.addEventListener("xuen:connected", connected)
		element.addEventListener("xuen:disconnected", disconnected)
		if (element.isConnected) connected(null)
		service
	}
}
