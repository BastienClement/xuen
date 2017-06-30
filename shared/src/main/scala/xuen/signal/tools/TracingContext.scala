package xuen.signal
package tools

import scala.collection.mutable
import scala.util.DynamicVariable

/***
  * A TracingContext is used to automatically detect signals used by an
  * arbitrary block of code.
  *
  * A new tracing context is created for every call to `trace`, accessing
  * the state of any mutable signal will cause it to register itself with
  * the tracing context. Once the evaluation of the block is complete, the
  * list of used signals is available in the context object.
  */
class TracingContext private {
	/** The set of signal used during execution of the traced block */
	private val signals = mutable.Buffer.empty[Mutable[_]]

	/**
	  * Registers a signal with the tracing context.
	  *
	  * This method is called by instances of `Mutable[_]` when their
	  * [[Mutable.option]] is called. This will cause the access to the
	  * signal to be recorded by the tracing context and is the basis of
	  * automatic signal-dependencies detection.
	  */
	private[signal] def record(signal: Mutable[_]): Unit = signals += signal
}

object TracingContext {
	/**
	  * The current, thread-local, instance of TracingContext.
	  * Is `null` if tracing is not requested.
	  */
	private[signal] val current = new DynamicVariable[TracingContext](null)

	/**
	  * Performs expression tracing.
	  *
	  * @param expr the expression to tracer
	  * @tparam T the type of result returned
	  * @return the result of evaluating the expression and a list of distinct
	  *         signals that were accessed during the expression evaluation
	  */
	def trace[T](expr: => T): (T, List[Mutable[_]]) = {
		val ctx = new TracingContext
		val res = current.withValue(ctx)(expr)
		(res, ctx.signals.distinct.toList)
	}

	/**
	  * A dummy tracing-context that doesn't track anything.
	  * Used as a substitute in case no tracing is enabled for the current thread.
	  */
	object DummyContext extends TracingContext {
		override private[signal] def record(signal: Mutable[_]): Unit = ()
	}

	/** Implicitly retrieves the current tracing context */
	implicit def implicitly: TracingContext = current.value match {
		case null => DummyContext
		case ctx => ctx
	}

	/** Alias for [[TracingContext.record]] without the need to explicitly conjure the implicit */
	private[signal] def record(signal: Mutable[_])(implicit ctx: TracingContext): Unit = ctx.record(signal)
}
