package xuen

import scala.util.control.ControlThrowable

package object signal {
	/**
	  * An exception thrown when accessing the current value of an undefined
	  * signal. This is usually caught by the [[Expression]] generator and
	  * indicates that the composed signal should also be undefined.
	  */
	case class UndefinedSignalException(signal: Signal[_]) extends NoSuchElementException with ControlThrowable

	/**
	  * An exception thrown when the generation of an [[Expression]]-defined
	  * signal causes an access to another generating [[Expression]], thus
	  * forming a circular dependency between two signals.
	  */
	case class CircularDependencyException() extends Throwable

	/** Implementation of an ordering based on object identity hash code */
	private[signal] object IdentityOrdering extends Ordering[AnyRef] {
		def compare(x: AnyRef, y: AnyRef): Int = Integer.compare(System.identityHashCode(x), System.identityHashCode(y))
		def forType[T <: AnyRef]: Ordering[T] = this.asInstanceOf[Ordering[T]]
	}

	sealed trait EvaluationMode

	object EvaluationMode {
		object Deferred extends EvaluationMode
		object Lazy extends EvaluationMode
	}
}
