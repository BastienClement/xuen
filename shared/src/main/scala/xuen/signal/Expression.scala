package xuen.signal

/**
  * An expression-defined signal.
  * This allow to turn arbitrary expressions into signals.
  *
  * Instances of this class must be created using the [[Signal.apply]] or
  * [[Signal.define]] methods.
  *
  * @param definition the definition expression
  * @tparam T the type of the signal
  */
class Expression[T] private[signal] (definition: => Option[T]) extends Lazy[T] {
	/** Generates the signal value from the definition expression */
	protected def generate: Option[T] = definition
}
