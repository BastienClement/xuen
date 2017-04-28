package xuen.expression

import scala.scalajs.js

trait Context {
	def has(key: String): Boolean
	def get(key: String): js.UndefOr[Any]
	def set(key: String, value: Any): Unit

	def invoke(key: String, args: Seq[Any]): Any = Context.performInvoke(invokeTarget, get(key), args)
	def invokeTarget: js.Dynamic

	def selectElement(selector: String): Any

	def child(properties: (String, Any)*): Context
}

object Context {
	private def performInvoke(target: js.Dynamic, func: Any, args: Seq[Any]): Any = func match {
		case f: js.Function => f.call(target, args.asInstanceOf[Seq[js.Any]]: _*)
		case _ => throw new IllegalArgumentException("Invoke target is not a function")
	}

	class Reference(val ref: js.Dynamic) extends Context {
		def has(key: String): Boolean = get(key).isDefined
		def get(key: String): js.UndefOr[Any] = ref.selectDynamic(key)
		def set(key: String, value: Any): Unit = get(key) match {
			case f: js.Function => throw new IllegalAccessException("Overriding a function property on the reference object is not allowed")
			case any if any.isDefined => ref.updateDynamic(key)(value.asInstanceOf[js.Any])
			case _ => throw new IllegalAccessException("Setting an undefined property on the reference object is not allowed")
		}
		def invokeTarget: js.Dynamic = ref
		def selectElement(selector: String): Any = ???
		def child(properties: (String, Any)*): Context = ???
	}
}
