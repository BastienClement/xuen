package xuen

import scala.util.control.ControlThrowable

package object signal {
	/**
	  * An exception thrown when accessing the current value of an undefined
	  * signal. This is usually caught by the [[Expression]] generator and
	  * indicates that the composed signal should also be undefined.
	  */
	case class UndefinedSignalException() extends NoSuchElementException with ControlThrowable

	/**
	  * An exception thrown when the generation of an [[Expression]]-defined
	  * signal causes an access to another generating [[Expression]], thus
	  * forming a circular dependency between two signals.
	  */
	case class CircularDependencyException() extends Throwable

	/**
	  * An object representing the "undefined" state of a signal. It can be
	  * passed to the `:=` operator of [[Source]] to erase the current value
	  * of the signal and set it undefined.
	  */
	case object UndefinedValue

	/** Implementation of an ordering based on object identity hash code */
	private[signal] object IdentityOrdering extends Ordering[AnyRef] {
		def compare(x: AnyRef, y: AnyRef): Int = System.identityHashCode(x) - System.identityHashCode(y)
		def forType[T <: AnyRef]: Ordering[T] = this.asInstanceOf[Ordering[T]]
	}
}
