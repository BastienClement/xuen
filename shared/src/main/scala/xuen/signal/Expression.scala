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
abstract class Expression[T] private (initialState: Option[T], parents: List[Mutable[_]], defn: => Option[T])
		extends Child[T](true, initialState, parents) {
	/** Generates the signal value from the definition expression */
	protected def generate: Option[T] = defn
}

object Expression {
	private[signal] class StrictExpr[T] (initialState: Option[T], parents: List[Mutable[_]], defn: => Option[T])
			extends Expression[T](initialState, parents, defn) with Strict[T]

	private[signal] class LazyExpr[T] (initialState: Option[T], parents: List[Mutable[_]], defn: => Option[T])
			extends Expression[T](initialState, parents, defn) with Lazy[T]
}
