package xuen.signal

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

case class Group (children: List[Group], observers: List[Observer], signals: List[Child[_]]) {
	def bindObservers(): Unit = {
		for (child <- children) child.bindObservers()
		for (observer <- observers) observer.bind()
	}

	def unbindObservers(): Unit = {
		for (child <- children) child.unbindObservers()
		for (observer <- observers) observer.unbind()
	}

	def invalidateSignals(disconnect: Boolean = false): Unit = {
		for (child <- children) child.invalidateSignals()
		if (disconnect) for (signal <- signals) signal.invalidate()(MutationContext.Dummy)
		else MutationContext.execute { implicit ctx => for (signal <- signals) signal.invalidate() }
	}
}

object Group {
	private val current = new DynamicVariable[Builder](null)
	private val empty = Group(Nil, Nil, Nil)

	private class Builder {
		private[Group] var empty = true
		private[Group] var children: ListBuffer[Group] = ListBuffer.empty
		private[Group] var observers: ListBuffer[Observer] = ListBuffer.empty
		private[Group] var signals: ListBuffer[Child[_]] = ListBuffer.empty

		def build(): Group = {
			if (empty) Group.empty
			else Group(children.toList, observers.toList, signals.toList)
		}
	}

	private[signal] def add(group: Group): Unit = add(group, _.children)
	private[signal] def add(observer: Observer): Unit = add(observer, _.observers)
	private[signal] def add(signal: Child[_]): Unit = add(signal, _.signals)

	@inline private def add[T](obj: T, collection: Builder => ListBuffer[T]): Unit = current.value match {
		case null => // Ignore
		case builder =>
			collection(builder) += obj
			builder.empty = false
	}

	private[signal] def build(block: => Unit): Group = {
		val builder = new Builder
		current.withValue(builder)(block)
		val group = builder.build()
		// Add to outer group if not empty
		if (group != Group.empty) Group.add(group)
		group
	}
}
