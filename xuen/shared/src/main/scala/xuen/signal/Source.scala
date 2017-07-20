package xuen.signal

import xuen.signal.tools.MutationContext

final class Source[T] private (initialValue: Option[T]) extends Mutable[T] {
	/** The current value of this signal */
	private[this] var currentValue: Option[T] = initialValue

	/** Simple proxy for currentValue to implement the Mutable interface */
	protected def current: Option[T] = currentValue

	/** Updates the current value of this signal */
	protected def current_= (value: Option[T]): Unit = MutationContext.execute { implicit ctx =>
		if (ctx.parents.contains(this)) {
			throw new IllegalStateException("Illegal reference to mutated Source during flush")
		}
		ctx.mutating(this)
		currentValue = value
		notifyUpdate()
	}

	/** Updates the current value of this signal */
	def := (value: T): Unit = current = Some(value)

	/**
	  * TODO
	  *
	  * This operation does not register the caller as a child of the signal.
	  * The given transformation function should not leak the source value.
	  *
	  * Note: without this, the ~= could not be used in observers without
	  * constructing an illegal cycle between the source and the observer.
	  *
	  * @param f
	  */
	def ~= (f: T => T): Unit = for (value <- currentValue) current = Some(f(value))

	/**
	  * Clears the value of this signal
	  *
	  * This method should be invoked as:
	  * `source := Source.nil`
	  *
	  * @param empty a dummy parameter that is never evaluated
	  */
	def := (empty: Signal.nil.type): Unit = current = None
}

object Source {
	/** Creates a new Source with the provided initial value */
	def apply[T](value: T): Source[T] = new Source(Some(value))

	/** Creates a new undefined Source */
	def undefined[T]: Source[T] = new Source(None)

	/** Alias for [[undefined]] */
	@inline def apply[T](): Source[T] = undefined[T]
}
