package xuen.signal
package tools

import scala.collection.mutable
import scala.util.DynamicVariable

class MutationContext private {
	private val deferred = mutable.Buffer.empty[Strict[_]]
	private val observers = mutable.Buffer.empty[Observer]

	private[signal] def defer(signal: Strict[_]): Unit = deferred += signal
	private[signal] def register(observer: Observer): Unit = observers += observer

	private def flush(): Unit = ()
}

object MutationContext {
	private val current = new DynamicVariable[MutationContext](null)

	def execute[T](block: => T): T = {
		if (current.value != null) block
		else {
			val ctx = new MutationContext
			try current.withValue(ctx)(block)
			finally ctx.flush()
		}
	}

	implicit def implicitly: MutationContext = current.value match {
		case null => throw new IllegalStateException("No active MutationContext for this thread")
		case ctx => ctx
	}
}
