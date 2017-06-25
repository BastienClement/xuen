package xuen.signal
import xuen.signal.tools.MutationContext

trait Strict[T] extends Child[T] {
	override protected[signal] def invalidate()(implicit mc: MutationContext): Unit = {
		super.invalidate()
		mc.defer(this)
	}
}
