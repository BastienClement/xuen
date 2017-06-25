package xuen.signal

/**
  * An signal that is always undefined.
  */
object Undefined extends Immutable[Nothing] {
	val option: Option[Nothing] = None
}
