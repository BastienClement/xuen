package xuen.signal

import scala.collection.immutable.TreeSet
import xuen.signal.tools.{MutationContext, TracingContext}

/**
  * Common trait for mutable signals.
  *
  * Mutable signals can have children that must be notified when their value
  * is updated. This is the common trait for both Sources and Lazies.
  *
  * @tparam T the type of value of the signal
  */
trait Mutable[T] extends Signal[T] {
	/** Current signal value, must be implemented by subclasses */
	protected def current: Option[T]

	/** Dependants-aware auto binding implementation for option */
	final def option: Option[T] = {
		TracingContext.record(this)
		current
	}

	/** The set of children dependant on this signal value for their own value */
	private[this] var children: Set[Child[_]] = Set.empty

	/** The list of observers bound to this signal */
	private[this] var observers: List[Observer] = Nil

	/** Attaches a new child signal */
	private[signal] final def attach(child: Child[_]): Unit = children += child

	/** Detaches a child signal */
	private[signal] final def detach(child: Child[_]): Unit = children -= child

	/** Invalidates signals that are dependant on this one */
	protected final def notifyUpdate()(implicit mc: MutationContext): Unit = {
		observers.foreach(mc.register)
		for (child <- children) child.invalidate()
		// No need to clear the `children` set since children will detach themselves on invalidate
	}
}
