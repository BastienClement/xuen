package xuen.signal

trait Immutable[+T] extends Signal[T] {
	// Override def to val, since the signal is immutable
	override val option: Option[T]
}
