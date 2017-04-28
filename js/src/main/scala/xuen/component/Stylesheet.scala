package xuen.component

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js
import scala.util.{Failure, Success, Try}
import xuen.facades.less.{Environment, LESS, Options, RenderResult}

case class Stylesheet (node: html.Style) {
	dom.console.log(node)
}

object Stylesheet {
	//noinspection TypeAnnotation
	def interpolate(sc: StringContext, args: Seq[Any]): Stylesheet = {
		require(sc.parts.length == 1 && args.isEmpty, "stylesheet interpolator does not support substitutions")
		val options = new Options {
			override val async = false
			override val syncImport = true
			override val env = Environment.production
		}
		var result: Option[Try[String]] = None
		LESS.render(sc.parts.head, options, (err: js.UndefOr[js.Any], res: js.UndefOr[RenderResult]) => {
			result = res.toOption.map(r => Success(r.css)).orElse {
				err.toOption.map(e => Failure(new RuntimeException(e.toString)))
			}
		})
		result match {
			case None => throw new RuntimeException("LESS compilation did not complete synchronously")
			case Some(Failure(e)) => throw e
			case Some(Success(css)) =>
				val node = dom.document.createElement("style").asInstanceOf[html.Style]
				node.textContent = css
				Stylesheet(node)
		}
	}

	implicit final class Interpolation(private val sc: StringContext) extends AnyVal {
		@inline def less(args: Any*): Stylesheet = interpolate(sc, args)
	}
}
