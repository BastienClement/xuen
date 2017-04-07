package xuen.signal

/**
  * An immutable signal whose value never change.
  *
  * @tparam T the type of value of the signal
  */
trait Immutable[+T] extends Signal[T] {
	// Override def to val, since the signal is immutable
	override val option: Option[T]
}
