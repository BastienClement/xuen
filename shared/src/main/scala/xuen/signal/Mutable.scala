package xuen.signal

import scala.collection.immutable.TreeSet

/**
  * Common trait for mutable signals.
  *
  * @tparam T the type of value of the signal
  */
trait Mutable[T] extends Signal[T] {
	/** Current signal value */
	protected def current: Option[T]

	/** The set of child dependant on this signal value for their own value */
	@volatile private[this] var children = TreeSet.empty[Lazy[_]](IdentityOrdering.forType)

	/** Attaches a new child signal */
	private[signal] def attach(child: Lazy[_]): Unit = synchronized {
		children += child
	}

	/** Detaches a child signal */
	private[signal] def detach(child: Lazy[_]): Unit = synchronized {
		children -= child
	}

	/** Invalidates signals that are dependant on this one */
	protected def invalidateChildren(): Unit = for (child <- children) child.invalidate()

	/** Dependants-aware auto binding implementation for option */
	final def option: Option[T] = {
		val child = Lazy.dynamic.value
		if (child != null) child.bind(this)
		current
	}

	def map[U](f: T => U): Signal[U] = Signal(f(value))
	def flatMap[U](f: (T) => Signal[U]): Signal[U] = Signal(f(value).value)
	def filter(p: T => Boolean): Signal[T] = Signal.define(option.filter(p))
}
