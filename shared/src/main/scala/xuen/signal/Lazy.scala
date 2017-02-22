package xuen.signal

import scala.collection.immutable.TreeSet
import scala.util.DynamicVariable

/**
  * A signal that lazily computes its value when required
  * @tparam T the type of the signal
  */
trait Lazy[T] extends Mutable[T] {
	private[this] var available: Boolean = false
	private[this] var generating: Boolean = false
	private[this] var currentValue: Option[T] = None
	private[this] var parents = TreeSet.empty[Mutable[_]](IdentityOrdering.forType)

	/** Generates a new value for this lazy signal */
	protected def generate: Option[T]

	protected final def current: Option[T] = {
		if (available) currentValue
		else {
			synchronized {
				if (!available) {
					if (generating) throw CircularDependencyException()
					else generating = true
					try currentValue = Lazy.dynamic.withValue(this)(generate)
					finally generating = false
					available = true
				}
				currentValue
			}
		}
	}

	private[signal] final def bind(parent: Mutable[_]): Unit = synchronized {
		parents += parent
		parent.attach(this)
	}

	final def invalidate(): Unit = synchronized {
		available = false
		currentValue = None
		for (parent <- parents) parent.detach(this)
		invalidateChildren()
	}
}

object Lazy {
	/** The dynamic enclosing lazy signal seen by Mutable[_] as a child */
	private[signal] val dynamic = new DynamicVariable[Lazy[_]](null)
}
