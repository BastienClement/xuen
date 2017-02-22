package xuen.signal

import scala.language.implicitConversions

trait Signal[+T] {
	/** Current value of this signal, if defined */
	def option: Option[T]

	final def isDefined: Boolean = option.isDefined
	final def isEmpty: Boolean = option.isEmpty

	/** Current value of this signal, throws if undefined */
	final def value: T = option match {
		case Some(value) => value
		case None => throw EmptySignalException()
	}

	def map[U](f: T => U): Signal[U]
	def flatMap[U](f: (T) => Signal[U]): Signal[U]
	def filter(p: T => Boolean): Signal[T]
}

object Signal {
	/** Constructs an empty signal ot type T */
	def empty[T]: Signal[T] = Empty

	/** Constructs a computed signal from the given generator */
	def apply[T](gen: => T): Signal[T] = new Expression(() => Some(gen))

	def define[T](gen: => Option[T]): Signal[T] = new Expression(() => gen)

	/** Wraps an acceptable input into signal */
	implicit def wrap[T, S](value: T)(implicit m: SignalMorphism[T, S]): Signal[S] = m.morph(value)
}
