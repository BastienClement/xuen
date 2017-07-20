package xuen.signal

/**
  * An expression-defined signal.
  * This allow to turn arbitrary expressions into signals.
  *
  * Instances of this class must be created using the [[Signal.apply]] or
  * [[Signal.define]] methods.
  *
  * @param defn the definition expression
  * @tparam T the type of the signal
  */
abstract class Expression[T] private (initialAvailability: Boolean, initialState: Option[T],
                                      parents: List[Mutable[_]], defn: => Option[T])
		extends Child[T](initialAvailability, initialState, parents) {
	/** Generates the signal value from the definition expression */
	protected def generate: Option[T] = try defn catch { case UndefinedSignalException(_) => None }
}

object Expression {
	private[signal] class DelayedExpr[T] (initialAvailability: Boolean, initialState: Option[T],
	                                      parents: List[Mutable[_]], defn: => Option[T])
			extends Expression[T](initialAvailability, initialState, parents, defn) with Deferred[T]

	private[signal] class LazyExpr[T] (initialAvailability: Boolean, initialState: Option[T],
	                                   parents: List[Mutable[_]], defn: => Option[T])
			extends Expression[T](initialAvailability, initialState, parents, defn) with Lazy[T]
}
