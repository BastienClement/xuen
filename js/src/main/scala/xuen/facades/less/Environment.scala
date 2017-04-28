package xuen.facades.less

import scala.scalajs.js

@js.native
sealed trait Environment extends js.Any

object Environment {
	val development: Environment = "development".asInstanceOf[Environment]
	val production: Environment = "production".asInstanceOf[Environment]
}
