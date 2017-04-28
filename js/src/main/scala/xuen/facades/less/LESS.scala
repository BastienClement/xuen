package xuen.facades.less

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("less")
object LESS extends js.Object {
	type RenderCallback = js.Function2[js.UndefOr[js.Any], js.UndefOr[RenderResult], Unit]
	def render(source: String, callback: RenderCallback): Unit = js.native
	def render(source: String, options: Options, callback: RenderCallback): Unit = js.native
	def render(source: String, options: js.UndefOr[Options] = js.undefined): js.Promise[RenderResult] = js.native
}
