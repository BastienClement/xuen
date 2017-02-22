package xuen.signal

/**
  * An always-empty signal
  */
object Undefined extends Immutable[Nothing] {
	val option: Option[Nothing] = None

	def map[U](f: (Nothing) => U): Undefined.type = this
	def flatMap[U](f: (Nothing) => Signal[U]): Undefined.type = this
	def filter(p: (Nothing) => Boolean): Undefined.type = this
}
