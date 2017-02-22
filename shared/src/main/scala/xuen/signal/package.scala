package xuen

import scala.util.control.ControlThrowable

package object signal {
	case class EmptySignalException() extends ControlThrowable
	case class CircularDependencyException() extends Throwable

	case object EmptyValue

	private[signal] object IdentityOrdering extends Ordering[AnyRef] {
		def compare(x: AnyRef, y: AnyRef): Int = x.hashCode() - x.hashCode()
		def forType[T <: AnyRef]: Ordering[T] = this.asInstanceOf[Ordering[T]]
	}
}
