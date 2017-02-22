package xuen.signal

import scala.collection.immutable.TreeSet
import scala.util.DynamicVariable

/**
  * A signal that lazily computes its value when required.
  *
  * Such signal has a list of parent signal from which its value is derived.
  * Any change in one of these signals will cause invalidation of the current
  * caches value and trigger a new generation the next time this signal value
  * is accessed.
  *
  * @tparam T the type of the signal
  */
trait Lazy[T] extends Mutable[T] {
	/** Whether a cached value for this signal is available */
	@volatile private[this] var available: Boolean = false

	/** Flag used to detect circular dependencies during generation */
	private[this] var generating: Boolean = false

	/** The current value of this signal */
	private[this] var currentValue: Option[T] = None

	/** The set of parent dependencies */
	private[this] var parents = TreeSet.empty[Mutable[_]](IdentityOrdering.forType)

	/**
	  * Generates a new value for this lazy signal.
	  *
	  * Any [[Mutable]] signals accessed during evaluation of this method will
	  * be automatically registered as dependencies of this signal.
	  *
	  * It is forbidden for two lazy signals to depends on each other. Circular
	  * dependencies during generation will fail with a [[CircularDependencyException]].
	  */
	protected def generate: Option[T]

	/**
	  * Returns the current value of this signal.
	  * If the value is not available, the [[generate]] method is called to
	  * generate a new value that is then cached.
	  */
	protected final def current: Option[T] = {
		// Attempt to avoid synchronization if possible
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

	/** Binds a parent mutable signal as a parent of this one */
	private[signal] final def bind(parent: Mutable[_]): Unit = synchronized {
		parents += parent
		parent.attach(this)
	}

	/**
	  * Invalidates this signal cached value and invalidate any children.
	  * Also detaches this signal from any parent signal to allow for garbage
	  * collection if this signal is no longer used.
	  */
	final def invalidate(): Unit = synchronized {
		if (available) {
			available = false
			currentValue = None
			for (parent <- parents) parent.detach(this)
			invalidateChildren()
		}
	}
}

object Lazy {
	/** The dynamic enclosing lazy signal seen by Mutable[_] as a child */
	private[signal] val dynamic = new DynamicVariable[Lazy[_]](null)
}
