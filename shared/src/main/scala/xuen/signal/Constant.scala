package xuen.signal

/**
  * A signal whose value does never change.
  * Monadic operators on this signal are computed eagerly.
  *
  * @param staticValue the value of the signal
  * @tparam T the type of value of this signal
  */
case class Constant[+T] (private val staticValue: T) extends Immutable[T] {
	val option: Option[T] = Some(staticValue)

	def map[U](f: (T) => U): Constant[U] = Constant(f(staticValue))
	def flatMap[U](f: (T) => Signal[U]): Signal[U] = f(staticValue)
	def filter(p: (T) => Boolean): Immutable[T] = if (p(staticValue)) this else Undefined
}
