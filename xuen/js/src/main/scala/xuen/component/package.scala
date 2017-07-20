package xuen

import xuen.template.Template

package object component {
	implicit final class Interpolations (private val sc: StringContext) extends AnyVal {
		@inline def html(args: Any*): Template = Template.interpolate(sc, args)
		@inline def css(args: Any*): Stylesheet = Stylesheet.interpolate(sc, args)
	}
}
