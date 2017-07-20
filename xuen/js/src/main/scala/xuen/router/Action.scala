package xuen.router

import org.scalajs.dom
import xuen.component.{Component, Element}
import xuen.router.{View => ViewElement}
import xuen.router.Router.Parameters

trait Action {
	private[router] def activate(router: Router, params: Parameters): Unit
	private[router] def deactivate(router: Router): Unit = ()
	private[router] def update(router: Router, params: Parameters): Unit = {deactivate(router); activate(router, params)}
}

object Action {
	case class View(component: Component[_ <: Element]) extends Action {
		private var instance: dom.Element = null

		private[router] def activate(router: Router, params: Parameters): Unit = {
			instance = component.instantiate()
			instance match {
				case view: ViewElement => view.params := params
				case _ => // No parameters passing
			}
			router.host.appendChild(instance)
		}

		private[router] override def deactivate(router: Router): Unit = {
			instance.parentNode.removeChild(instance)
			instance = null
		}
	}

	case class Redirect(url: String, replace: Boolean = true) extends Action {
		private[router] def activate(router: Router, params: Parameters): Unit = Router.goto(url, replace)
	}
}
