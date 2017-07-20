package xuen.signal

import xuen.signal.tools.{MutationContext, TracingContext}

/**
  * A signal that lazily computes its value when requested.
  *
  * Such signal has a list of parent signal from which its value is derived.
  * Any change in one of these signals will cause invalidation of the current
  * cached value and trigger a new generation the next time the value of this
  * signal is accessed.
  *
  * @tparam T the type of the signal
  */
abstract class Child[T] (private[this] var available: Boolean = false,
                         private[this] var currentState: Option[T] = None,
                         private[this] var parents: List[Mutable[_]] = Nil) extends Mutable[T] {
	// Attach child to parents
	parents.foreach(_ attach this)

	// Add child to open group
	Group.add(this)

	/** Flag used to detect circular dependencies during generation */
	private[this] var generating: Boolean = false

	/**
	  * Generates a new value for this lazy signal.
	  * Any [[Mutable]] signals accessed during evaluation of this method will
	  * be automatically registered as dependencies of this signal.
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
		if (available) currentState
		else {
			if (generating) throw CircularDependencyException()
			else generating = true
			try {
				val (res, ps) = TracingContext.trace(generate)
				currentState = res
				parents = ps
				parents.foreach(_ attach this)
			} finally generating = false
			available = true
			currentState
		}
	}

	/**
	  * Invalidates this signal cached value and invalidate any children.
	  * Also detaches this signal from any parent signal to allow for garbage
	  * collection if this signal is no longer used.
	  */
	protected[signal] def invalidate()(implicit mc: MutationContext): Unit = {
		if (available) {
			available = false
			currentState = None
			parents.foreach(_ detach this)
			parents = Nil
			notifyUpdate()
		}
	}
}
