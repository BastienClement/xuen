package xuen.expression

import scala.scalajs.js
import scala.scalajs.js.DynamicImplicits.truthValue
import scala.scalajs.js.JSConverters._
import xuen.expression.Expression._

object Interpreter {
	def evaluate(expr: Expression)(implicit context: Context): Any = {
		val value = evaluateTree(expr)
		// TODO: resolve signals
		value
	}

	private def evaluateTree(expr: Expression)(implicit context: Context): Any = expr match {
		case Empty => js.undefined
		case ImplicitReceiver => ImplicitReceiver

		case Chain(expressions) => evaluateChain(expressions)
		case Conditional(cond, yes, no) => evaluateConditional(cond, yes, no)
		case PropertyRead(receiver, name, safe) => evaluatePropertyRead(receiver, name, safe)
		case PropertyWrite(receiver, name, value) => evaluatePropertyWrite(receiver, name, value)
		case FunctionCall(target, args) => evaluateFunctionCall(target, args)
		case Binary(op, lhs, rhs) => evaluateBinary(op, lhs, rhs)
		case Unary(op, operand) => evaluateUnary(op, operand)
		case Range(from, to, step) => evaluateRange(from, to, step)
		case SelectorQuery(id) => evaluateSelectorQuery(id)
		case LiteralPrimitive(value) => value
		case LiteralArray(values) => evaluateLiteralArray(values)
		case LiteralObject(values) => evaluateLiteralObject(values)
		case Interpolation(fragments) => evaluateInterpolation(fragments)

		case Enumerator(_, _, _, _, _, _) => ???
	}

	private def evaluateChain(expressions: Seq[Expression])
	                         (implicit context: Context): Any = {
		evaluate(expressions.reduceLeft { (a: Expression, b: Expression) => evaluate(a); b })
	}

	private def evaluateConditional(cond: Expression, yes: Expression, no: Expression)
	                               (implicit context: Context): Any = {
		if (evaluate(cond).asInstanceOf[js.Dynamic]) evaluate(yes) else evaluate(no)
	}

	private def evaluatePropertyRead(receiver: Expression, property: Expression, safe: Boolean)
	                                (implicit context: Context): Any = {
		evaluatePropertyReadReceiver(receiver, property, safe) match {
			case (_, value) => value
		}
	}

	private def evaluatePropertyReadReceiver(receiver: Expression, property: Expression, safe: Boolean)
	                                        (implicit context: Context): (js.Dynamic, Any) = {
		val key = evaluate(property).toString
		evaluate(receiver) match {
			case ImplicitReceiver => (null, context.get(key))
			case target if js.isUndefined(target) && safe => (null, js.undefined)
			case target =>
				val dynTarget = target.asInstanceOf[js.Dynamic]
				(dynTarget, dynTarget.selectDynamic(key))
		}
	}

	private def evaluatePropertyWrite(receiver: Expression, property: Expression, value: Expression)
	                                 (implicit context: Context): Any = {
		val key = evaluate(property).toString
		val effectiveValue = evaluate(value)
		evaluate(receiver) match {
			case ImplicitReceiver => context.set(key, effectiveValue)
			case target => target.asInstanceOf[js.Dynamic].updateDynamic(key)(effectiveValue.asInstanceOf[js.Any])
		}
		value
	}

	private def evaluateFunctionCall(target: Expression, args: Seq[Expression])
	                                (implicit context: Context): Any = {
		val effectiveArgs = args.map(evaluate(_).asInstanceOf[js.Any])
		target match {
			case PropertyRead(ImplicitReceiver, property, _) =>
				context.invoke(evaluate(property).toString, effectiveArgs)

			case PropertyRead(receiver, property, safe) =>
				evaluatePropertyReadReceiver(receiver, property, safe) match {
					case (_, fn) if js.isUndefined(fn) =>
						if (safe) js.undefined
						else throw new IllegalAccessError("Invoke target is undefined")
					case (thisp, fn: js.Function) =>
						fn.call(thisp, effectiveArgs: _*)
					case _ =>
						throw new IllegalAccessError("Invoke target is not a function")
				}

			case _ =>
				evaluate(target) match {
					case f: js.Function => f.call(null, effectiveArgs: _*)
					case _ => throw new IllegalAccessError("Invoke target is not a function")
				}
		}
	}

	private def evaluateBinary(op: String, lhs: Expression, rhs: Expression)
	                          (implicit context: Context): Any = {
		@inline def effectiveLhs = evaluate(lhs).asInstanceOf[js.Dynamic]
		@inline def effectiveRhs = evaluate(rhs).asInstanceOf[js.Dynamic]
		op match {
			case "+" => effectiveLhs + effectiveRhs
			case "-" => effectiveLhs - effectiveRhs
			case "*" => effectiveLhs * effectiveRhs
			case "%" => effectiveLhs % effectiveRhs
			case "/" => effectiveLhs / effectiveRhs
			case "<" => effectiveLhs < effectiveRhs
			case "<=" => effectiveLhs <= effectiveRhs
			case ">" => effectiveLhs > effectiveRhs
			case ">=" => effectiveLhs >= effectiveRhs
			case "==" => effectiveLhs == effectiveRhs
			case "!=" => effectiveLhs != effectiveRhs
			case "===" => effectiveLhs eq effectiveRhs
			case "!==" => effectiveLhs ne effectiveRhs
			case "&&" => effectiveLhs && effectiveRhs
			case "||" => effectiveLhs || effectiveRhs
		}
	}

	private def evaluateUnary(op: String, operand: Expression)
	                         (implicit context: Context): Any = {
		val effectiveOperand = evaluate(operand).asInstanceOf[js.Dynamic]
		op match {
			case "+" => effectiveOperand.unary_+()
			case "-" => effectiveOperand.unary_-()
			case "!" => effectiveOperand.unary_!()
		}
	}

	private def evaluateRange(from: Expression, to: Expression, step: Option[Expression])
	                         (implicit context: Context): Any = {
		val lower = (evaluate(from).asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
		val upper = (evaluate(to).asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
		val effectiveStep = step.map(evaluate).map { value =>
			(value.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
		}.getOrElse {
			if (lower > upper) -1 else 1
		}
		lower to upper by effectiveStep
	}

	private def evaluateSelectorQuery(selector: String)
	                                 (implicit context: Context): Any = {
		context.selectElement(selector)
	}

	private def evaluateLiteralArray(values: Seq[Expression])
	                                (implicit context: Context): Any = {
		values.map { value => evaluate(value).asInstanceOf[js.Any] }.toJSArray
	}

	private def evaluateLiteralObject(values: Seq[(Expression, Expression)])
	                                 (implicit context: Context): Any = {
		values.map {
			case (key, value) => (evaluate(key).toString, evaluate(value).asInstanceOf[js.Any])
		}.toMap.toJSDictionary
	}

	private def evaluateInterpolation(fragments: Seq[InterpolationFragment])(implicit context: Context): Any = {
		fragments.map {
			case StringFragment(str) => str
			case ExpressionFragment(e) =>
				val value = evaluate(e)
				if (value == null) "null"
				else value.toString
		}.mkString
	}
}
