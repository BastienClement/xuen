package xuen.template

import xuen.expression.Context

case class TemplateInstance(template: Template, context: Context) {
	private[this] var $state: Boolean = false

	def state: Boolean = $state

	def enable(): Unit = ???
	def disable(): Unit = ???
}
