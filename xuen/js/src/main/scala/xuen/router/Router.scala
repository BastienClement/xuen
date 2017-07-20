package xuen.router

import org.scalajs.dom
import scala.scalajs.js
import xuen.signal.{Signal, Source}
import xuen.utils.Event.CustomDispatch

class Router (val host: dom.Element)(routes: (Route, Action)*) {
	private val currentAction: Source[Action] = Source.undefined
	private val handler: js.Function1[dom.PopStateEvent, Unit] = { _ => dispatch() }

	/** Exposes current action as a signal */
	val action: Signal[Action] = currentAction

	/** Start listening to events */
	def start(): Unit = {
		dom.window.addEventListener("popstate", handler)
		dom.document.addEventListener("xuen:router:dispatch", handler)
		dispatch()
	}

	def stop(): Unit = {
		dom.window.removeEventListener("popstate", handler)
		dom.document.removeEventListener("xuen:router:dispatch", handler)
	}

	def goto(url: String, replace: Boolean = false): Unit = Router.goto(url, replace)

	private def dispatch(): Unit = {
		routes.iterator.map {
			case (rte, act) => (rte.matching, act)
		}.collectFirst {
			case (Some(params), act) => (params, act)
		} match {
			case None =>
				for (a <- currentAction.option) a.deactivate(this)
				currentAction := Signal.nil
			case Some((params, act)) if currentAction.option.contains(act) =>
				currentAction.value.update(this, params)
			case Some((params, act)) =>
				for (a <- currentAction.option) a.deactivate(this)
				currentAction := act
				act.activate(this, params)
		}
	}
}

object Router {
	type Parameter = js.UndefOr[String]
	type Parameters = Vector[js.UndefOr[String]]

	def apply(host: dom.Element)(rules: (Route, Action)*): Router = new Router(host)(rules: _*)

	def goto(url: String, replace: Boolean = false): Unit = {
		if (replace) dom.window.history.replaceState(null, "", url)
		else dom.window.history.pushState(null, "", url)
		dom.document.dispatchEvent("xuen:router:dispatch")
	}
}
