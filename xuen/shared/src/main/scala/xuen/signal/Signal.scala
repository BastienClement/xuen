package xuen.signal

import scala.language.{higherKinds, implicitConversions}
import xuen.signal.tools.{MutationContext, TracingContext}

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
		case None => throw UndefinedSignalException(this)
	}

	/**
	  * Creates a new signal whose value will be the result of applying
	  * a function to this signal current value.
	  *
	  * @param f the function to apply to this signal value
	  * @tparam U the type of value of the new signal
	  */
	final def map[U](f: T => U): Signal[U] = Signal.define(option.map(f))

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
	final def flatMap[U](f: (T) => Signal[U]): Signal[U] = Signal.define(option.map(f(_).value))

	/**
	  * Creates a new signal that will have the same value as this signal when
	  * the given predicate holds, but will be undefined when it does not.
	  *
	  * @param p the predicate function
	  */
	final def filter(p: T => Boolean): Signal[T] = Signal.define(option.filter(p))
	final def withFilter(p: T => Boolean): Signal[T] = filter(p)

	final def collect[U](pf: PartialFunction[T, U]): Signal[U] = Signal.define(pf.lift(value))

	final def fold[U](initial: U)(f: (U, T) => U): Signal[U] = {
		var last = initial
		Signal.apply({last = option.map(f(last, _)).getOrElse(last); last}, EvaluationMode.Deferred)
	}

	final def reduce[U >: T](f: (U, T) => U): Signal[U] = {
		fold(None: Option[U])((prev, cur) => prev.map(p => f(p, cur)).orElse(Some(cur))).unwrap
	}

	final def wrap: Signal[Option[T]] = Signal.apply(option)

	final def unwrap[U](implicit ev: T <:< Option[U]): Signal[U] = Signal.define(ev(value))

	final def flatten[U](implicit ev: T <:< Signal[U]): Signal[U] = flatMap(ev)

	final def foreach[U](f: T => U): Observer = Observer(option.foreach(f))
}

object Signal {
	/**
	  * TODO
	  */
	object nil extends UndefinedSignalException(null) {
		override def toString: String = "<undefined signal>"
	}

	implicit def autoThrowNil(n: nil.type): Nothing = throw n

	/**
	  * Constructs an undefined signal ot type T.
	  *
	  * @tparam T the type of the signal
	  * @return the [[Undefined]] signal singleton
	  */
	def undefined[T]: Signal[T] = Undefined

	/**
	  * Constructs a computed signal from the given expression.
	  * Dependencies will be automatically detected and bound.
	  *
	  * @param defn the definition expression
	  * @tparam T the type of the signal
	  */
	def apply[T](defn: => T, mode: EvaluationMode = EvaluationMode.Lazy, defer: Boolean = false): Signal[T] = {
		define(Some(defn), mode, defer)
	}

	def defer[T](defn: => T, mode: EvaluationMode = EvaluationMode.Lazy): Signal[T] = apply(defn, mode, defer = true)

	/**
	  * Defines a computed signal from the given expression.
	  * Behave like [[apply]] except that the generator is expected to
	  * return an Option instead of a value of type T.
	  *
	  * THE ULTIMATE SIGNAL BUILDER
	  *
	  * @param defn the definition expression
	  * @tparam T the type of the signal
	  */
	def define[T](defn: => Option[T], mode: EvaluationMode = EvaluationMode.Lazy, defer: Boolean = false): Signal[T] = {
		if (defer) {
			mode match {
				case EvaluationMode.Lazy => new Expression.LazyExpr[T](false, None, Nil, defn)
				case EvaluationMode.Deferred => new Expression.DeferredExpr[T](false, None, Nil, defn)
			}
		} else {
			TracingContext.trace(try defn catch {case UndefinedSignalException(_) => None}) match {
				case (None, Nil) => Undefined
				case (Some(value), Nil) => Constant(value)
				case (state, parents) => mode match {
					case EvaluationMode.Lazy => new Expression.LazyExpr[T](true, state, parents, defn)
					case EvaluationMode.Deferred => new Expression.DeferredExpr[T](true, state, parents, defn)
				}
			}
		}
	}

	/**
	  * Executes a block atomically.
	  *
	  * Deferred signal evaluation and observers trigger will be delayed until
	  * the end of the block. As such, every mutations will be applied once
	  * deferred signals and observers are evaluated.
	  *
	  * @param block
	  * @tparam T
	  * @return
	  */
	def atomically[T](block: => T): T = MutationContext.execute(_ => block)

	def grouped(block: => Unit): Group = Group.build(block)
}
