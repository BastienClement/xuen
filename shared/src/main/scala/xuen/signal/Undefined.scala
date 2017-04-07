package xuen.signal

/**
  * An signal that is always undefined.
  * Monadic operation on the undefined signal are no-ops.
  */
object Undefined extends Immutable[Nothing] {
	val option: Option[Nothing] = None

	@inline def map[U](f: (Nothing) => U): Undefined.type = this
	@inline def flatMap[U](f: (Nothing) => Signal[U]): Undefined.type = this
	@inline def filter(p: (Nothing) => Boolean): Undefined.type = this
}
