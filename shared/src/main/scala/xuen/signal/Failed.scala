package xuen.signal

case class Failed (cause: Throwable) extends Immutable[Nothing] {
	val option: Option[Nothing] = throw new IllegalStateException("failed signal", cause)
}
