package xuen.signal
import xuen.signal.tools.MutationContext

trait Deferred[T] extends Child[T] {
	override protected[signal] def invalidate()(implicit mc: MutationContext): Unit = {
		super.invalidate()
		mc.defer(this)
	}
}
