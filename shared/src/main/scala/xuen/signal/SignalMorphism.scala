package xuen.signal

import scala.language.higherKinds

trait SignalMorphism[-In, +S[_], T] {
	def morph(source: In): S[T]
}

trait LowPrioritySignalMorphisms {
	// Signal from anything
	private[this] type WrapperMorphism[T] = SignalMorphism[T, Constant, T]
	private[this] val WrapperMorphism = new WrapperMorphism[Any] {
		def morph(source: Any): Constant[Any] = Constant(source)
	}
	@inline implicit final def Wrapper[T]: WrapperMorphism[T] = WrapperMorphism.asInstanceOf[WrapperMorphism[T]]
}

object SignalMorphism extends LowPrioritySignalMorphisms {
	// Signal from Option
	private[this] type OptionMorphism[T] = SignalMorphism[Option[T], Immutable, T]
	private[this] val OptionMorphism = new OptionMorphism[Any] {
		def morph(source: Option[Any]): Immutable[Any] = source match {
			case Some(value) => Constant(value)
			case None => Undefined
		}
	}
	@inline implicit final def FromOption[T]: OptionMorphism[T] = OptionMorphism.asInstanceOf[OptionMorphism[T]]
}
