package xuen.expression

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import xuen.signal.Source

/**
  * A context is used during expression evaluation. It represent the
  * environment in which the expression is evaluated and is queried whenever
  * the expression references global variables of functions.
  */
trait Context {
	def self: js.Dynamic
	def selector: js.UndefOr[String => dom.Element]

	def has(key: String): Boolean
	def get(key: String): js.UndefOr[Any]
	def set(key: String, value: Any): Unit

	def invoke(key: String, args: Seq[Any]): Any
	def selectElement(selector: String): Any
}

object Context {
	/**
	  * Constructs a child context
	  *
	  * @param parent     the parent context
	  * @param self       the self reference for this context
	  * @param selector   the select root of the child
	  * @param properties a list of properties to set on the context
	  */
	def child(parent: Context, self: js.Dynamic, selector: js.UndefOr[String => dom.Element],
	          properties: (String, Any)*): Context = {
		val ctx = new Context.Child(parent, self, selector)
		for ((k, v) <- properties) ctx.set(k, v)
		ctx
	}

	/** Performs function invocation */
	private def performInvoke(target: js.Dynamic, func: Any, name: String, args: Seq[Any]): Any = func match {
		case f: js.Function => f.call(target, args.asInstanceOf[Seq[js.Any]]: _*)
		case _ => throw new IllegalArgumentException(s"Invoke target '$name' is not a function")
	}

	/**
	  * A root context with a JavaScript object as `self`.
	  *
	  * @param self a JavaScript object
	  */
	class Root(val self: js.Dynamic, val selector: js.UndefOr[String => dom.Element] = js.undefined) extends Context {
		def has(key: String): Boolean = get(key).isDefined
		def get(key: String): js.UndefOr[Any] = self.selectDynamic(key)
		def set(key: String, value: Any): Unit = {
			println(key, value)
			get(key) match {
				case f: js.Function => throw new IllegalAccessException("Overriding a function property on the reference object is not allowed")
				case any if any.isDefined =>
					println(any.get)
					any.get match {
						case source: Source[_] => source.asInstanceOf[Source[Any]] := value
						case _ => self.updateDynamic(key)(value.asInstanceOf[js.Any])
					}
				case _ => throw new IllegalAccessException("Setting an undefined property on the reference object is not allowed")
			}
		}

		def invoke(key: String, args: Seq[Any]): Any = {
			Context.performInvoke(self, get(key), key, args)
		}

		def selectElement(query: String): Any = {
			if (selector.isEmpty) throw new UnsupportedOperationException("Context.selectElement on a context with no selector")
			else selector.get.apply(query) match {
				case null => throw new NoSuchElementException(s"Selector '$selector' did not match any element")
				case element => element
			}
		}
	}

	/**
	  * A child context that delegates actions to its parent in case it can
	  * not be handled locally. This mechanism is used to build scoping.
	  *
	  * The [[has]] and [[get]] methods will call the corresponding parent
	  * method if the key is not defined on this context. The [[set]] method
	  * will always set the key locally, potentially shadowing a parent
	  * property with the same name.
	  *
	  * The [[invoke]] will forward invocation to the parent context if no
	  * matching key is defined on this context. It will not check the actual
	  * type of value associated with the invoked key. Shadowing a function
	  * by another value of a non-function type will cause a type error.
	  *
	  * The [[selectElement]] will attempt to find a matching element from
	  * this context [[selector]]. If the select root is undefined or no
	  * matching element is found, the search is forwarded to the parent
	  * context. This way, selectors have a shadowing semantic matching the
	  * one of property names.
	  *
	  * @param parent   the parent context
	  * @param self     the self property of this context
	  * @param selector the select root for this context
	  */
	class Child(val parent: Context, val self: js.Dynamic, val selector: js.UndefOr[String => dom.Element]) extends Context {
		private[this] val locals = js.Object.create(null).asInstanceOf[js.Dictionary[Any]]

		def has(key: String): Boolean = locals.contains(key) || parent.has(key)
		def get(key: String): UndefOr[Any] = locals.get(key).getOrElse(parent.get(key))
		def set(key: String, value: Any): Unit = locals.put(key, value)

		override def invoke(key: String, args: Seq[Any]): Any = {
			if (locals.contains(key)) Context.performInvoke(self, get(key), key, args)
			else parent.invoke(key, args)
		}

		override def selectElement(query: String): Any = {
			selector.map(_.apply(query)).getOrElse(null) match {
				case null => parent.selectElement(query)
				case element => element
			}
		}
	}

	/**
	  * A dummy context used by the optimizer to evaluate static expressions.
	  *
	  * Implementations of methods inherited from [[Context]] by this object are
	  * stubs throwing UnsupportedOperationException.
	  */
	object Dummy extends Context {
		def self: js.Dynamic = throw new UnsupportedOperationException("Dummy.self")
		def selector: js.UndefOr[String => dom.Element] = throw new UnsupportedOperationException("Dummy.selectRoot")
		def has(key: String): Boolean = false
		def get(key: String): UndefOr[Any] = throw new UnsupportedOperationException("Dummy.get")
		def set(key: String, value: Any): Unit = throw new UnsupportedOperationException("Dummy.set")
		def invoke(key: String, args: Seq[Any]): Any = throw new UnsupportedOperationException("Dummy.invoke")
		def selectElement(selector: String): Any = throw new UnsupportedOperationException("Dummy.selectElement")
	}
}
