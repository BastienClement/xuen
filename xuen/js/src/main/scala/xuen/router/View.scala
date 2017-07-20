package xuen.router

import xuen.component.{AttributeFormat, Component, Element}
import xuen.router.Router.Parameters
import xuen.signal.{Signal, Source}

abstract class View (component: Component[_]) extends Element(component) {
	/**
	  * The set of route parameters passed by the router.
	  *
	  * This property should not be used directly. Instead, the [[param]]
	  * method should be used to construct a well-typed signal bound to the
	  * parameter value.
	  */
	final val params = Source.undefined[Parameters]

	/**
	  * Defines a route parameter binding.
	  *
	  * The parameter is given by the router as String. An implicit formatter
	  * of type T is used to convert this value to the expected type T.
	  *
	  * @param idx the parameter index
	  * @param fmt a formatter for the requested type T
	  * @tparam T the type of value of the binding
	  */
	def param[T](idx: Int)(implicit fmt: AttributeFormat[T]): Signal[T] = params.map { ps =>
		if (0 < idx && idx <= ps.length) ps.apply(idx - 1).map(fmt.decode).toOption
		else None
	}.unwrap
}
