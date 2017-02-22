package xuen.signal

/**
  * An always-empty signal
  */
object Empty extends Signal[Nothing] {
	val option: Option[Nothing] = None

	def map[U](f: (Nothing) => U): Empty.type = this
	def flatMap[U](f: (Nothing) => Signal[U]): Signal[U] = this
	def filter(p: (Nothing) => Boolean): Empty.type = this
}
