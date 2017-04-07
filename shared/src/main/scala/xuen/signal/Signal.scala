package xuen.signal

import scala.language.{higherKinds, implicitConversions}

/**
  * A Signal is a container for a value that can change with time.
  *
  * Signals offer a monadic-style API to transform its value in a functional
  * way and ensure that change are propagated through the dependency network.
  *
  * Signals also offer an implicit way to declare new Signal with automatic
  * dependencies detection.
  *
  * @tparam T the type of value of the signal
  */
trait Signal[+T] {
	/** Current value of this signal, None if undefined */
	def option: Option[T]

	/** Whether the signal currently has a defined value */
	final def isDefined: Boolean = option.isDefined

	/** Whether the signal is currently undefined */
	final def isUndefined: Boolean = option.isEmpty

	/**
	  * Returns the current value of the signal, if defined. If the signal
	  * is undefined an [[UndefinedSignalException]] is thrown instead.
	  */
	final def value: T = option match {
		case Some(value) => value
		case None => throw UndefinedSignalException()
	}

	/**
	  * Creates a new signal whose value will be the result of applying
	  * a function to this signal current value.
	  *
	  * @param f the function to apply to this signal value
	  * @tparam U the type of value of the new signal
	  */
	def map[U](f: T => U): Signal[U]

	/**
	  * Creates a new signal whose value will be the same as the signal
	  * returned by a function applied to this signal current value.
	  *
	  * This method implements a switch-like behavior, where the current value
	  * of this signal is used to select another signal and return it value.
	  *
	  * @param f the function to apply to this signal value
	  * @tparam U the type of the value of the new signal
	  */
	def flatMap[U](f: (T) => Signal[U]): Signal[U]

	/**
	  * Creates a new signal that will have the same value as this signal when
	  * the given predicate holds, but will be undefined when it does not.
	  *
	  * @param p the predicate function
	  */
	def filter(p: T => Boolean): Signal[T]
}

object Signal {
	/**
	  * Throws the [[UndefinedSignalException]] exception.
	  *
	  * This can be used to return from expression defined signals and result
	  * in an undefined signal.
	  */
	def nil: Nothing = throw UndefinedSignalException()

	/**
	  * Constructs an undefined signal ot type T.
	  *
	  * @tparam T the type of the signal
	  * @return the [[Undefined]] signal singleton
	  */
	def undefined[T]: Undefined.type = Undefined

	/**
	  * Constructs a computed signal from the given expression.
	  * Dependencies will be automatically detected and bound.
	  *
	  * @param expr the definition expression
	  * @tparam T the type of the signal
	  */
	def apply[T](expr: => T): Expression[T] = new Expression(Some(expr))

	/**
	  * Defines a computed signal from the given expression.
	  * Behave like [[apply]] except that the generator is expected to
	  * return an [[Option]] instead of a value of type [[T]].
	  *
	  * @param definition the definition expression
	  * @tparam T the type of the signal
	  */
	def define[T](definition: => Option[T]): Expression[T] = new Expression(definition)

	/** Wraps a constant value inside a signal */
	implicit def wrap[T](value: T): Constant[T] = Constant(value)

	/** Wraps an optional constant value inside a signal */
	implicit def fromOption[T](option: Option[T]): Immutable[T] = option match {
		case Some(value) => Constant(value)
		case None => Undefined
	}
}
