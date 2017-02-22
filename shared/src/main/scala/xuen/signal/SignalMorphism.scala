package xuen.signal

import scala.concurrent.{ExecutionContext, Future}

trait SignalMorphism[-In, T] {
	def morph(source: In): Signal[T]
}

trait LowPrioritySignalMorphisms {
	// Signal from anything
	trait WrapperMorphism[T] extends SignalMorphism[T, T] {
		def morph(source: T): Signal[T] = Constant(source)
	}
	object WrapperMorphism extends WrapperMorphism[Nothing]
	@inline implicit final def Wrapper[T]: SignalMorphism[T, T] = WrapperMorphism.asInstanceOf[WrapperMorphism[T]]
}

object SignalMorphism extends LowPrioritySignalMorphisms {
	// Signal from Option
	trait OptionMorphism[T] extends SignalMorphism[Option[T], T] {
		def morph(source: Option[T]): Signal[T] = source match {
			case Some(value) => Constant(value)
			case None => Empty
		}
	}
	object OptionMorphism extends OptionMorphism[Nothing]
	@inline implicit final def FromOption[T]: SignalMorphism[Option[T], T] = OptionMorphism.asInstanceOf[OptionMorphism[T]]

	// Signal from Future
	@inline implicit final def FromFuture[T](implicit ec: ExecutionContext) = new SignalMorphism[Future[T], T] {
		def morph(source: Future[T]): Signal[T] = {
			val signal = Source.empty[T]
			source.foreach(value => signal := value)
			signal
		}
	}
}
