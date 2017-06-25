package xuen.signal

/**
  * A signal whose value never change.
  * Monadic operators on this signal are computed eagerly.
  *
  * @param staticValue the value of the signal
  * @tparam T the type of value of this signal
  */
case class Constant[+T] (private val staticValue: T) extends Immutable[T] {
	val option: Option[T] = Some(staticValue)
}
