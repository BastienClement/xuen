package xuen.template

import org.scalajs.dom
import org.scalajs.dom.ext.PimpedNodeList
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.DynamicImplicits.truthValue
import xuen.expression.{Context, Expression, ExpressionError, Interpreter}
import xuen.expression.Expression.LiteralPrimitive
import xuen.facades.webcomponents.HTMLTemplateElement
import xuen.signal.{Observer, Signal, Source}

/**
  * The template compiler.
  *
  * The compiler is responsible for turning a HTMLTemplateElement node to
  * an instance of [[Template]]. This is done by recursively walking the
  * template DOM tree and looking for data-binding elements.
  */
object Compiler {
	/**
	  * Compiles an HTMLTemplateElement and produces a Xuen [[Template]]
	  * instance that can be instantiated at a later time.
	  *
	  * @param node the root template node
	  * @return a new instance of [[Template]]
	  */
	def compile(node: HTMLTemplateElement): Template = compile(node, None)

	private def compile(node: HTMLTemplateElement, parent: Option[Template]): Template = {
		implicit val template = Template(node, parent)
		node.content.normalize()
		if (node.content.childNodes.length < 1) {
			val comment = node.ownerDocument.createComment(" empty template ")
			node.content.appendChild(comment)
		}
		compileNode(node.content)
		template
	}

	/**
	  * Compiles a DOM node. This is the main method of the compiler, checking
	  * the node type and performing dispatch to other sub-compilation methods.
	  *
	  * @param node the node being compiled
	  * @param tpl  the template object
	  */
	private def compileNode(node: dom.Node)(implicit tpl: Template): Unit = node match {
		case element: dom.Element => compileElement(element)
		case text: dom.Text => compileText(text)
		case comment: dom.Comment => compileComment(comment)
		case fragment: dom.DocumentFragment => compileDocumentFragment(fragment)
		case unknown =>
			val tpe = unknown.nodeType
			throw new IllegalStateException(s"Encountered unsupported node type `$unknown` ($tpe) while compiling template")
	}

	/**
	  * Compiles an element node.
	  *
	  * @param element the element node being compiled
	  * @param tpl     the template object
	  */
	private def compileElement(element: dom.Element)(implicit tpl: Template): Unit = {
		// If this is a custom element, record it
		if (element.tagName.contains("-")) tpl.dependencies += element.tagName.toLowerCase

		// Compile transformations and children
		if (element.hasAttribute("*if")) compileIfTransformation(element)
		else if (element.hasAttribute("*for")) compileForTransformation(element)
		else {
			if (element.hasAttributes) compileAttributes(element)
			element.childNodes.toList.foreach(node => compileNode(node))
		}
	}

	/** Compiles attributes of an element */
	private def compileAttributes(element: dom.Element)(implicit tpl: Template): Unit = {
		val attributes = for (i <- 0 until element.attributes.length) yield element.attributes.item(i)
		for (attr <- attributes; name = attr.name; value = Option(attr.value).filter(v => v.trim.nonEmpty)) {
			(name.head, name.last) match {
				case ('#', _) => compileIdSugarAttribute(element, name)
				case ('.', _) => compileClassSugarAttribute(element, name, value)
				case ('[', ']') => compilePropertyBindingAttribute(element, name, value)
				case ('(', ')') => compileEventListenerAttribute(element, name, value)
				case _ => compileAttributeInterpolation(element, name, value)
			}
		}
	}

	/** Compiles id attribute sugar */
	private def compileIdSugarAttribute(element: dom.Element, name: String)(implicit tpl: Template): Unit = {
		element.removeAttribute(name)
		element.setAttribute("id", name.substring(1))
	}

	/**
	  * Compiles class attribute sugar
	  *
	  * If no value is present, the class is unconditionally added to the
	  * `classList` of the element.
	  *
	  * If a value is given, it will be interpreted as a predicate boolean
	  * expression. If the predicate holds, then the class will be added to
	  * the class list, otherwise the class will be removed.
	  */
	private def compileClassSugarAttribute(element: dom.Element, name: String, value: Option[String])
	                                      (implicit tpl: Template): Unit = {
		val className = name.substring(1)
		if (value.isEmpty) element.classList.add(className)
		else element.attachAttributeExpression(name) { (el, value, _) =>
			if (value.asInstanceOf[js.Dynamic]) el.classList.add(className)
			else el.classList.remove(className)
		}
	}

	/** Compiles a property data-binding attribute */
	private def compilePropertyBindingAttribute(element: dom.Element, name: String, value: Option[String])
	                                           (implicit tpl: Template): Unit = {
		val propertyName = name.substring(1, name.length - 1)
		if (value.isEmpty) element.getAttributeNode(name).value = propertyName
		element.attachAttributeExpression(name) { (el, value, _) =>
			el.asInstanceOf[js.Dynamic].selectDynamic(propertyName).asInstanceOf[Any] match {
				case source: Source[_] => source.asInstanceOf[Source[Any]] := value
				case _ => el.asInstanceOf[js.Dynamic].updateDynamic(propertyName)(value.asInstanceOf[js.Any])
			}
		}
	}

	/** Compiles an event listener attribute */
	private def compileEventListenerAttribute(element: dom.Element, name: String, value: Option[String])
	                                         (implicit tpl: Template): Unit = {
		val eventName = name.substring(1, name.length - 1)
		element.removeAttribute(name)
		value.map(Expression.parse) match {
			case None => dom.console.warn("Defined event listener without a body: ", name, element)
			case Some(Left(err)) => dom.console.error(err.toString, element)
			case Some(Right(expression)) =>
				element.attachBehavior { (el, instance) =>
					val context = Context.child(instance.context, instance.context.self, js.undefined)
					el.addEventListener(eventName, (event: Any) => {
						context.set("event", event)
						Interpreter.evaluate(expression)(context)
					})
				}
		}
	}

	/** Compiles a simple attribute interpolation */
	private def compileAttributeInterpolation(element: dom.Element, name: String, value: Option[String])
	                                         (implicit tpl: Template): Unit = {
		Expression.parseInterpolation(value getOrElse name) match {
			case Left(err) => dom.console.error(err.toString, element)
			case Right(None) => // No interpolation found
			case Right(Some(LiteralPrimitive(str: String))) => element.setAttribute(name, str)
			case Right(Some(expression)) =>
				element.attachExpression(expression)((c, v, _) => c.setAttribute(name, v.toString))
		}
	}

	/** Compiles an `if` transformation */
	private def compileIfTransformation(element: dom.Element)(implicit tpl: Template): Unit = {
		compileAttributeExpression(element, "*if") { (source, expression) =>
			val (child, placeholder) = templateWrap(element, "*if " + source)
			placeholder.attachBehavior { (ph, parent) =>
				// The predicate signal
				val predicate = Signal.defer {
					Interpreter.evaluate(expression)(parent.context).asInstanceOf[js.Dynamic]
				}

				val instance = child.instantiate(parent.context)
				var mounted = false

				Observer.unbound {
					val predicateHolds = predicate.value
					if (!mounted && predicateHolds) {
						instance.mount(ph.parentNode, ph.nextSibling)
						ph.parentNode.removeChild(ph)
						parent.attach(instance)
						mounted = true
					} else if (mounted && !predicateHolds) {
						parent.detach(instance)
						instance.nodes.head.parentNode.insertBefore(ph, instance.nodes.head)
						instance.unmount()
						mounted = false
					}
				}
			}
		}
	}

	/**
	  * Compiles a `for` transformation.
	  *
	  * The *for attribute is parsed as an enumerator and used to retrieve the
	  * set of items to iterate over. Any subclasses of [[Iterable]] or [[js.Array]]
	  * can be used as a source for iteration. Unless the given collection is a
	  * [[Map]], each element will be zipped with their indices to form a collection
	  * of (key, value) pairs. In the case of a map, the existing map key is used.
	  *
	  * An enumerator definition is composed of various part that are evaluated
	  * with various context during the construction of the DOM tree:
	  *
	  * for="[{key},] {value} of {iterable} [by {id}] [if {filter}] [with {locals...}]"
	  *
	  * - The {iterable} part is evaluated first to retrieve the iterated object.
	  * It is evaluated in the enclosing scope context.
	  * - The next part to be evaluated is the "if clause", if defined. This clause is
	  * evaluated in a child context and can access `key` and `value`.
	  * - Then, if the filter is missing or successful, the "by clause" is evaluated to
	  * identify the node that is currently being updated. If the clause is missing,
	  * the index is used to identify the row. During this evaluation, the additional
	  * properties `$index`, `$first` and `$last` are available.
	  * - Finally the "with clause" is evaluated. As a result, anything defined in
	  * this clause is not available for the definition of the enumerator itself, but
	  * only available to child elements created by the *for transformation.
	  *
	  * @param element the element on which the *for attribute is defined
	  * @param tpl     the template object
	  */
	private def compileForTransformation(element: dom.Element)(implicit tpl: Template): Unit = {
		compileAttributeExpression(element, "*for", Expression.parseEnumerator) { (source, enumerator) =>
			val (child, placeholder) = templateWrap(element, "*for " + source)
			placeholder.attachBehavior { (ph, parent) =>
				val source = Signal.defer[Iterable[(Any, Any)]] {
					def asIterable(col: Any): Iterable[_] = col match {
						case it: Iterable[_] => it
						case ar: js.Array[_] => ar
						case ar: Array[_] => ar
						case _: Unit | null => Nil
						case unsupported =>
							dom.console.error(s"Unsupported iterable in for-loop: ${unsupported.getClass.getName}")
							Nil
					}
					asIterable(Interpreter.evaluate(enumerator.iterable)(parent.context)) match {
						case map: Map[_, _] => map
						case it: Iterable[_] => Stream.from(0).zip(it)
					}
				}

				case class ForNode (var index: Int, context: Context) {
					var flag = false
					var fresh = true
					val template = child.instantiate(context)
				}

				var nodes: Map[Any, ForNode] = Map.empty
				val keyName = enumerator.key.getOrElse("$key")

				Observer.unbound {
					// Mark all nodes ready for disposal
					for ((_, node) <- nodes) node.flag = true

					var previousNode: dom.Node = ph
					val items = source.value
					val lastIndex = items.size - 1
					var index = 0

					for {
						((key, value), rawIndex) <- items.zipWithIndex
						baseCtx = {
							// Base context only includes value and key from the enumerator, used for filtering
							// This convoluted construct is used to only instantiate the context if required, if
							// the enumerator does not use filtering or arbitrary identifiers, there is no need
							// to instantiate a context before building or retrieving the item node.
							lazy val ctx = Context.child(parent.context, parent.context.self, js.undefined,
								enumerator.value -> Source(value),
								keyName -> Source(key)
							)
							() => ctx
						}
						if enumerator.filter.forall(Interpreter.evaluate(_)(baseCtx()).asInstanceOf[js.Dynamic])
					} {
						// Extend base ctx with indexes data, used for identifier evaluation
						lazy val ctx = {
							val base = baseCtx()
							base.set("$index", Source(index))
							base.set("$first", Source(index == 0))
							base.set("$last", Source(rawIndex == lastIndex))
							base
						}

						// Compute node identifier
						val id = enumerator.by.map(Interpreter.evaluate(_)(ctx)).getOrElse(index)

						// Fetch node object or instantiate it
						val node = nodes.getOrElse(id, {
							val n = ForNode(index, ctx)
							nodes += (id -> n)
							n
						})

						// Update old node context if reused
						if (!node.fresh) {
							val old = node.context
							old.get(enumerator.value).asInstanceOf[Source[Any]] := value
							old.get(keyName).asInstanceOf[Source[Any]] := key
							if (node.index != index) {
								old.get("$index").asInstanceOf[Source[Int]] := index
								old.get("$first").asInstanceOf[Source[Boolean]] := (index == 0)
								old.get("$last").asInstanceOf[Source[Boolean]] := (rawIndex == lastIndex)
								node.index = index
							}
						} else {
							node.fresh = false
						}

						// Evaluate locals
						for (locals <- enumerator.locals) {
							Interpreter.evaluate(locals)(node.context)
						}

						// Clear the dispose flag
						node.flag = false

						// Update the dom
						if (previousNode.nextSibling ne node.template.nodes.head) {
							node.template.mount(previousNode.parentNode, previousNode.nextSibling)
							parent.attach(node.template)
						}

						previousNode = node.template.nodes.last
						index += 1
					}

					for ((key, node) <- nodes if node.flag) {
						node.template.unmount()
						parent.detach(node.template)
						nodes -= key
					}
				}
			}
		}
	}

	/** Compiles a text node */
	private def compileText(text: dom.Text)(implicit tpl: Template): Unit = {
		compileInterpolation(text, _.ownerDocument.createTextNode(""))
	}

	/** Compiles a comment node */
	private def compileComment(comment: dom.Comment)(implicit tpl: Template): Unit = {
		compileInterpolation(comment, _.ownerDocument.createComment(""))
	}

	/**
	  * Compiles interpolation in a non-element node.
	  * This is a general implementation covering both text and comment nodes.
	  *
	  * @param node the DOM node
	  * @param mat  a builder for node of the correct type
	  * @param tpl  the template object
	  */
	private def compileInterpolation(node: dom.Node, mat: dom.Element => dom.Node)(implicit tpl: Template): Unit = {
		Expression.parseInterpolation(node.textContent) match {
			case Left(err) => dom.console.error(err.toString, node)
			case Right(None) => // No interpolation found
			case Right(Some(LiteralPrimitive(str: String))) => node.textContent = str
			case Right(Some(expression)) =>
				createPlaceholder(node, mat).attachExpression(expression)((c, v, _) => c.textContent = v.toString)
		}
	}

	/** Compiles a DocumentFragment node */
	private def compileDocumentFragment(fragment: dom.DocumentFragment)(implicit tpl: Template): Unit = {
		// Start by copying refs to nodes to prevent one compilation to affect others
		val nodes = for (i <- 0 until fragment.childNodes.length) yield fragment.childNodes(i)
		nodes.foreach(node => compileNode(node))
	}

	/**
	  * Compiles an attribute expression and removes the attribute from the element.
	  *
	  * TODO
	  *
	  * @param element   the element on which the transformation is applied
	  * @param attribute the transformation attribute name
	  * @param builder   the implementation of the transformation
	  * @param tpl       the template object
	  */
	private def compileAttributeExpression[E <: Expression](element: dom.Element, attribute: String,
	                                                        parser: (String) => Either[ExpressionError, E] = Expression.parse _)
	                                                       (builder: (String, E) => Unit)
	                                                       (implicit tpl: Template): Unit = {
		val expression = element.getAttribute(attribute)
		parser(expression) match {
			case Left(err) => dom.console.error(s"Error while compiling $attribute expression:", err.toString, element)
			case Right(ast) =>
				element.removeAttribute(attribute)
				builder(expression, ast)
		}
	}

	/** The last generated behavior id */
	private var lastBehaviorId = 0

	/**
	  * Generates a new behavior id.
	  *
	  * Behavior IDs are sequential alphanumeric identifiers used as value to
	  * the `xuen:behavior` attribute and as key in the `behaviors` property
	  * of Templates. When the template is instantiated, this identifier is
	  * used to find nodes for which a behavior if defined by using a CSS
	  * attribute selector.
	  *
	  * @return a new, unique, behavior id
	  */
	private[template] def nextBehaviorId: String = {
		lastBehaviorId += 1
		Integer.toString(lastBehaviorId, Character.MAX_RADIX)
	}

	/**
	  * Wrap the given element in a <template> tag and compile that child
	  * template. If the given element is already a template element, no
	  * wrapping is performed and sub template is compiled as is.
	  *
	  * The template element is then replaced in the DOM tree by a Comment
	  * node with the given text, so that the template isn't cloned with the
	  * rest of the tree when the parent template is instantiated.
	  *
	  * @param element the element to wrap
	  * @param tpl     the template object
	  * @return the compiled child template and the placeholder
	  */
	private def templateWrap(element: dom.Element, text: String = "<template placeholder>")
	                        (implicit tpl: Template): (Template, Placeholder[dom.Comment]) = {
		val template = element match {
			case wrapper: HTMLTemplateElement if !wrapper.hasAttributes =>
				// Reuse existing template element
				wrapper
			case _ =>
				// Wrap element in a template tag and update the DOM tree
				val wrapper = element.ownerDocument.createElement("template").asInstanceOf[HTMLTemplateElement]
				element.parentNode.replaceChild(wrapper, element)
				wrapper.content.appendChild(element)
				wrapper
		}
		val placeholder = createPlaceholder(template, _.ownerDocument.createComment(s" $text "))
		val compiled = compile(template, Some(tpl))
		(compiled, placeholder)
	}

	/**
	  * Creates a placeholder element in place of a non-element DOM node.
	  *
	  * This is required as to be able to tag the element with a behavior-id
	  * as CSS selectors can only select element nodes. Such elements are
	  * associated with a materializer function that will replace the
	  * placeholder by the correct node type on template instantiation.
	  *
	  * @param node the node to replace
	  * @param mat  a builder for the correct node type
	  * @param tpl  the template object
	  * @tparam N the type of node replaced
	  * @return a placeholder for this node
	  */
	private def createPlaceholder[N <: dom.Node](node: dom.Node, mat: dom.Element => N)
	                                            (implicit tpl: Template): Placeholder[N] = {
		val placeholder = node.ownerDocument.createElement("xuen:placeholder")
		node.parentNode.replaceChild(placeholder, node)
		val id = nextBehaviorId
		placeholder.setAttribute("xuen:behavior", id)
		tpl.behaviors += (id -> Behavior(mat))
		placeholder.asInstanceOf[Placeholder[N]]
	}

	/**
	  * Implicit class adding [[attachBehavior]] and [[attachExpression]] on
	  * types for which an implicit [[WithBehavior]] instance exists.
	  *
	  * @param target the target DOM node
	  * @param wb     a instance of [[WithBehavior]] for the given target
	  * @tparam T the type of the target node
	  * @tparam N the type of DOM node associated with the target
	  */
	implicit private class BehaviorTarget[T, N <: dom.Node](private val target: T)(implicit wb: WithBehavior[T, N]) {
		/**
		  * Attaches a behavior to a DOM node. This can also be called on an
		  * instance of [[Placeholder]], in which case the behavior is bound
		  * to the runtime instance of the node.
		  *
		  * The behavior will be build for each instance of the template by
		  * invoking the builder function.
		  *
		  * @param builder the behavior builder function
		  * @param tpl     the template object
		  */
		@inline final def attachBehavior(builder: Behavior.Builder[N])(implicit tpl: Template): Unit = {
			wb.getBehavior(target).builders += builder
		}

		/**
		  * Attaches an expression behavior to a DOM node.
		  * This will invoke the [[Interpreter]] with the given expression
		  * and use an observer to detect updates. Each time the value of
		  * the expression changes, the given function will be called with
		  * both the DOM node and the value of the expression.
		  *
		  * @param expression the expression to bind
		  * @param f          a function called when the value changes
		  * @param tpl        the template object
		  */
		@inline final def attachExpression(expression: Expression)(f: (N, Any, TemplateInstance) => Unit)
		                                  (implicit tpl: Template): Unit = {
			attachBehavior { (node, instance) =>
				Observer.unbound {
					f(node, Interpreter.evaluate(expression)(instance.context), instance)
				}
			}
		}

		/**
		  * Todo
		  *
		  * @param attribute
		  * @param f
		  * @param tpl
		  * @param constraint
		  */
		@inline final def attachAttributeExpression(attribute: String)(f: (N, Any, TemplateInstance) => Unit)
		                                           (implicit tpl: Template, constraint: T =:= dom.Element): Unit = {
			compileAttributeExpression(target, attribute)((_, e) => attachExpression(e)(f))
		}
	}
}
