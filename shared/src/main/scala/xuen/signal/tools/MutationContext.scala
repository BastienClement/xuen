package xuen.signal
package tools

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Try}

/**
  * Every changes to source signals are bound to a MutationContext that keep
  * track of mutated signals and triggered observers. Evaluation of deferred
  * child signals and observers is delayed until the call to [[flush]].
  *
  * The flush operation also constructs a new mutation context. Any mutation
  * performed by the observers of the current set of signals are considered
  * atomic and will only trigger observers of the next set of signals once.
  *
  * Note: call nesting between mutation context, sources and observer can grow
  * quickly. Even if unlikely, a stack overflow error that should be thrown by
  * a normal use case (finite number of mutations) could be fixed by using a
  * trampoline between flush calls, limiting nesting to a single level.
  * See: [https://en.wikipedia.org/wiki/Trampoline_(computing)]
  *
  * @param parents the set of signal mutated in the parent context
  */
class MutationContext private (val parents: Set[Mutable[_]] = Set.empty) {
	/** The set of signals mutated during this context */
	private var mutated: Set[Mutable[_]] = Set.empty

	/** The set of deferred signals to be evaluated on flush */
	private val deferred = mutable.Buffer.empty[Deferred[_]]

	/** The set of observers to be called on flush */
	private val observers = mutable.Buffer.empty[Observer]

	/** Registers a new signal as being mutated during this context */
	private[signal] def mutating(signal: Mutable[_]): Unit = mutated += signal

	/** Registers a deferred signals to be evaluated on flush */
	private[signal] def defer(signal: Deferred[_]): Unit = deferred += signal

	/** Registers an observer to be called on flush */
	private[signal] def register(obs: Set[Observer]): Unit = observers ++= obs

	/**
	  * Flushes this mutation context.
	  *
	  * This operation involves evaluating every deferred signal that are
	  * children of mutated signals and then calling every observers bound
	  * to mutated signals.
	  *
	  * This is an atomic operation. A new mutation context will be open and
	  * any mutation caused by observers will be semantically applied as one
	  * operation. Further deferred signals and observers will only be called
	  * once.
	  *
	  * Any exception occurring during flush will be collected and rethrow as
	  * a [[MutationContext.MutationException]] once the process is complete.
	  * An exception thrown by a deferred signals or an observer does not
	  * prevent other signal and observer evaluation. This feature is intended
	  * as a debug mechanism and throwing exceptions from signals or observers
	  * should be avoided. They should instead be caught and handled locally.
	  */
	private def flush(): Unit = MutationContext.execute(mutated, { _ =>
		val se = deferred.distinct.map(signal => Try(signal.option)).collect { case Failure(e) => e }
		val oe = observers.distinct.map(observer => Try(observer.trigger())).collect { case Failure(e) => e }
		(se ++ oe).toList match {
			case Nil => ()
			case es => throw MutationContext.MutationException(es)
		}
	})
}

object MutationContext {
	/** The current mutation context for this thread */
	private val current = new DynamicVariable[MutationContext](null)

	/**
	  * Executes the given block in the current mutation context.
	  * If no context exists for the current thread, a new one is created.
	  *
	  * When the outermost call to [[execute]] returns, the mutation context
	  * is flushed, evaluating deferred signal and calling observers.
	  *
	  * @param block the block to execute
	  * @tparam T the return type of the block
	  * @return the return value of the block
	  */
	def execute[T](block: MutationContext => T): T = execute(Set.empty, block)

	/**
	  * Internal variant of execute, taking the set of mutated signals from
	  * parent context as parameter.
	  *
	  * @param parents the set of signal mutated in the parent context
	  * @param block   the block to execute
	  * @tparam T the return type of the block
	  * @return the return value of the block
	  */
	private def execute[T](parents: Set[Mutable[_]], block: MutationContext => T): T = {
		if (current.value != null) {
			// Nested call, do nothing and call block
			block(current.value)
		} else {
			// Outermost call
			val ctx = new MutationContext(parents)
			try current.withValue(ctx)(block(ctx))
			finally if (ctx.deferred.nonEmpty || ctx.observers.nonEmpty) ctx.flush()
		}
	}

	private def buildExceptionMessage(causes: List[Throwable]): String = causes match {
		case e :: Nil => "Exception occurred during flushing"
		case es => "Multiple exceptions occurred during flushing: " +
		           es.map(_.getMessage).mkString("\n- ", "\n- ", "")
	}

	/**
	  * A meta-exception collecting exceptions thrown by deferred signals and
	  * observers during mutation context flushing.
	  *
	  * This is intended as a feedback mechanism for mutation operations during
	  * debug only. It is conceptually wrong to handle consumer errors (exceptions
	  * throw by observer) on the producer side (mutation operations).
	  *
	  * @param causes the list of thrown exceptions
	  */
	case class MutationException(causes: List[Throwable])
			extends Exception(buildExceptionMessage(causes), causes.head) {
		/** The number of thrown exception */
		val count = causes.size
	}
}
