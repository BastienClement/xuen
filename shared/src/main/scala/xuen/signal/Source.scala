package xuen.signal

class Source[T] private (initialValue: Option[T]) extends Mutable[T] {
	/** The current value of this signal */
	private[this] var currentValue: Option[T] = initialValue

	/** Simple proxy for currentValue to implement Mutable interface */
	protected def current: Option[T] = currentValue

	/** Updates the current value of this signal */
	protected def current_= (value: Option[T]): Unit = {
		currentValue = value
		invalidateChildren()
	}

	/** Updates the current value of this signal */
	def := (value: T): Unit = current = Some(value)

	/** Clears the value of this signal */
	def := (empty: EmptyValue.type): Unit = current = None
}

object Source {
	def apply[T](value: T): Source[T] = new Source(Some(value))
	def apply[T](): Source[T] = empty[T]
	def empty[T]: Source[T] = new Source(None)
}
