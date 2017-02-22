package xuen.signal

/**
  * A signal that never changes value
  *
  * @param staticValue the value of the signal
  * @tparam T the type of value of this signal
  */
case class Constant[+T] (private val staticValue: T) extends Signal[T] {
	override val option: Option[T] = Some(staticValue)

	def map[U](f: (T) => U): Signal[U] = Constant(f(staticValue))
	def flatMap[U](f: (T) => Signal[U]): Signal[U] = f(staticValue)
	def filter(p: (T) => Boolean): Signal[T] = if (p(staticValue)) this else Empty
}
