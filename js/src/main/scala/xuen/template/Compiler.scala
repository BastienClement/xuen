package xuen.template

import org.scalajs.dom
import xuen.facades.webcomponents.HTMLTemplateElement

object Compiler {
	case class CompilationUnit(root: HTMLTemplateElement, debugName: String) {

	}

	def compile(node: HTMLTemplateElement, debugName: String = "?"): CompilationUnit = {
		implicit val unit = CompilationUnit(node, debugName)
		node.content.normalize()
		if (node.content.childNodes.length < 1) {
			val comment = node.ownerDocument.createComment(" empty template ")
			node.content.appendChild(comment)
		}
		compileNode(node.content)
		unit
	}

	private def compileNode(node: dom.Node)(implicit unit: CompilationUnit): Unit = node match {
		case element: dom.Element => compileElement(element)
		case text: dom.Text => compileText(text)
		case comment: dom.Comment => compileComment(comment)
		case fragment: dom.DocumentFragment => compileFragment(fragment)
		case unknown =>
			val tpe = unknown.nodeType
			throw new IllegalStateException(s"Encountered unsupported node type `$unknown` ($tpe) while " +
			                                s"compiling template ${unit.debugName}")
	}

	private def compileElement(element: dom.Element)(implicit unit: CompilationUnit): Unit = ???
	private def compileText(text: dom.Text)(implicit unit: CompilationUnit): Unit = ???
	private def compileComment(comment: dom.Comment)(implicit unit: CompilationUnit): Unit = ???

	private def compileFragment(fragment: dom.DocumentFragment)(implicit unit: CompilationUnit): Unit = {
		// TODO: check behavior if compilation of one child affects child indexes
		for (i <- 0 until fragment.childNodes.length) {
			compileNode(fragment.childNodes(i))
		}
	}
}
