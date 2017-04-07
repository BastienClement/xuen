package xuen.signal

class Source[T] private (initialValue: Option[T]) extends Mutable[T] {
	/** The current value of this signal */
	private[this] var currentValue: Option[T] = initialValue

	/** Simple proxy for currentValue to implement the Mutable interface */
	protected def current: Option[T] = currentValue

	/** Updates the current value of this signal */
	protected def current_= (value: Option[T]): Unit = {
		currentValue = value
		invalidateChildren()
	}

	/** Updates the current value of this signal */
	def := (value: T): Unit = current = Some(value)

	/** Clears the value of this signal */
	def := (empty: UndefinedValue.type): Unit = current = None
}

object Source {
	/** Creates a new Source with the provided initial value */
	def apply[T](value: T): Source[T] = new Source(Some(value))

	/** Creates a new undefined Source */
	def undefined[T]: Source[T] = new Source(None)

	/** Alias for [[undefined]] */
	@inline def apply[T](): Source[T] = undefined[T]
}
