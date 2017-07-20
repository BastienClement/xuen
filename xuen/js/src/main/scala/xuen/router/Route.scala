package xuen.router

import org.scalajs.dom
import scala.language.implicitConversions
import scala.scalajs.js.RegExp
import xuen.router.Router.Parameters

trait Route {
	def matching: Option[Parameters]
}

object Route {
	case class Pattern (pattern: String) extends Route {
		val regExp = new RegExp(pattern)
		def matching: Option[Parameters] = {
			regExp.exec(dom.window.location.pathname) match {
				case null => None
				case res => Some(res.jsSlice(1).toVector)
			}
		}
	}

	object Default extends Route {
		def matching: Option[Parameters] = Some(Vector.empty)
	}
}
