package xuen.signal

class Expression[T] private[signal] (gen: () => Option[T]) extends Lazy[T] {
	protected def generate: Option[T] = gen()
}
