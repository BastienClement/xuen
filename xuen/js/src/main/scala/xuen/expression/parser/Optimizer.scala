package xuen.expression
package parser

import scala.scalajs.js
import scala.scalajs.js.DynamicImplicits.truthValue
import scala.scalajs.js.JSConverters._
import scala.util.Try
import xuen.expression.Expression._

private[expression] object Optimizer {
	/** Attempts to find known compile time value */
	private def staticValue(expression: Expression): Option[Any] = expression match {
		case Empty => Some(js.undefined)

		case Chain(exprs) =>
			if (exprs.isEmpty) Some(js.undefined)
			else staticValue(exprs.last)

		case Conditional(cond, yes, no) =>
			staticTruth(cond) match {
				case Some(true) => staticValue(yes)
				case Some(false) => staticValue(no)
				case None =>
					for (y <- staticValue(yes); n <- staticValue(no); if y == n) yield y
			}

		case expr @ PropertyRead(receiver, property, _) =>
			for {
				_ <- staticValue(property)
				_ <- staticValue(receiver)
			} yield Interpreter.evaluate(expr.copy(safe = true))(Context.Dummy)

		case PropertyWrite(_, _, value) => staticValue(value)

		case Binary(op, lhs, rhs) =>
			for {
				left <- staticValue(lhs)
				right <- staticValue(rhs)
				result <- staticBinary(op, left.asInstanceOf[js.Dynamic], right.asInstanceOf[js.Dynamic])
			} yield result

		case Unary(op, operand) =>
			for {
				value <- staticValue(operand)
				result <- staticUnary(op, value.asInstanceOf[js.Dynamic])
			} yield result

		case Range(from, to, step) =>
			(staticValue(from), staticValue(to), step.flatMap(staticValue)) match {
				case (Some(a: Int), Some(b: Int), Some(c: Int)) => Some(a to b by c)
				case (Some(a: Int), Some(b: Int), None) => Some(a to b)
				case _ => None
			}

		case StringView(expr) =>
			staticValue(expr) map {
				case null => "null"
				case value => value.toString
			}

		case LiteralPrimitive(value) => Some(value)

		case LiteralArray(values) =>
			val materialized = values.map(staticValue)
			if (materialized.forall(_.isDefined)) Some(materialized.map(_.get).toJSArray)
			else None

		case LiteralObject(values) =>
			val materialized = values.map {
				case (k, v) => for (key <- staticValue(k); value <- staticValue(v)) yield (key.toString, value)
			}
			if (materialized.forall(_.isDefined)) Some(materialized.map(_.get).toMap.toJSDictionary)
			else None

		case _ => None
	}

	/** Attempts to find known compile time truth value */
	private def staticTruth(expression: Expression): Option[Boolean] = expression match {
		case Empty => Some(false)

		case Chain(exprs) => if (exprs.isEmpty) Some(false) else staticTruth(exprs.last)

		case Conditional(cond, yes, no) if staticTruth(cond).isEmpty =>
			for (y <- staticTruth(yes); n <- staticTruth(no); if y == n) yield y

		case PropertyWrite(_, _, value) => staticTruth(value)

		case Binary(op @ ("||" | "&&"), lhs, rhs) =>
			(op, staticTruth(lhs), staticTruth(rhs)) match {
				case ("||", _, Some(true)) | ("||", Some(true), _) => Some(true)
				case ("||", lhsTruth, Some(false)) => lhsTruth
				case ("||", Some(false), rhsTruth) => rhsTruth

				case ("&&", _, Some(false)) | ("&&", Some(false), _) => Some(false)
				case ("&&", lhsTruth, Some(true)) => lhsTruth
				case ("&&", Some(true), rhsTruth) => rhsTruth

				case _ => None
			}

		case LiteralArray(_) | LiteralObject(_) | StringView(_) => Some(true)

		case _ => staticValue(expression).map(_.asInstanceOf[js.Dynamic]: Boolean)
	}

	/* An expression is pure if it does not have any visible side effects */
	private def pure(expression: Expression): Boolean = expression match {
		case Empty => true
		case ImplicitReceiver => false

		case Chain(exprs) => exprs.forall(pure)

		case Conditional(cond, yes, no) => pure(cond) && (staticTruth(cond) match {
			case Some(true) => pure(yes)
			case Some(false) => pure(no)
			case None => pure(yes) && pure(no)
		})

		case Binary(_, lhs, rhs) => pure(lhs) && pure(rhs)
		case Unary(_, operand) => pure(operand)

		case LiteralArray(values) => values.forall(pure)
		case LiteralObject(values) => values.forall { case (key, value) => pure(key) && pure(value) }

		case Interpolation(fragments) => fragments.forall {
			case StringFragment(_) => true
			case ExpressionFragment(expr) => pure(expr)
		}

		case Range(from, to, step) => pure(from) && pure(to) && step.forall(pure)

		case SelectorQuery(_) | LiteralPrimitive(_) => true
		case StringView(expr) => pure(expr)

		case PropertyRead(LiteralPrimitive(_), _, _) => true

		case _ => false
	}

	/** Executes a binary operation at compile time */
	private def staticBinary(op: String, lhs: js.Dynamic, rhs: js.Dynamic): Option[Any] = Try(op match {
		case "+" => lhs + rhs
		case "-" => lhs - rhs
		case "*" => lhs * rhs
		case "%" => lhs % rhs
		case "/" => lhs / rhs
		case "&&" => lhs && rhs
		case "||" => lhs || rhs
		case ">" => lhs > rhs
		case ">=" => lhs >= rhs
		case "<" => lhs < rhs
		case "<=" => lhs <= rhs
		case "==" => lhs == rhs
		case "!=" => lhs != rhs
		case "===" => lhs eq rhs
		case "!==" => lhs ne rhs
	}).toOption

	/** Executes a unary operation at compile time */
	private def staticUnary(op: String, operand: js.Dynamic): Option[Any] = Try(op match {
		case "+" => operand.unary_+()
		case "-" => operand.unary_-()
		case "!" => operand.unary_!()
	}).toOption

	/** Joins sequential StringFragments in the interpolation */
	private def normalizeInterpolation(fragments: List[InterpolationFragment]): List[InterpolationFragment] = fragments match {
		case Nil => fragments
		case StringFragment(a) :: StringFragment(b) :: tail => normalizeInterpolation(StringFragment(a + b) :: tail)
		case head :: tail => head :: normalizeInterpolation(tail)
	}

	private def precomputeFragment(expr: Expression): Option[StringFragment] = {
		if (pure(expr)) {
			staticValue(expr).map {
				case null => StringFragment("null")
				case value => StringFragment(value.toString)
			}
		} else {
			None
		}
	}

	/** Optimizes the given expression */
	def optimize(expression: Expression): Expression = {
		optimizeTree(expression) match {
			case node @ LiteralPrimitive(_) => node
			case node if pure(node) => staticValue(node).map(LiteralPrimitive).getOrElse(node)
			case node => node
		}
	}

	/** Performs tree-optimization son the given expression */
	def optimizeTree(expression: Expression): Expression = expression match {
		case Chain(expressions) =>
			if (expressions.isEmpty) {
				Empty
			} else {
				val optimized = expressions.map(optimize)
				val filteredInit = optimized.init.filter(!pure(_))
				if (filteredInit.isEmpty) optimized.last
				else Chain(filteredInit :+ optimized.last)
			}

		case Conditional(cond, yes, no) =>
			val optCond = optimize(cond)
			lazy val optYes = optimize(yes)
			lazy val optNo = optimize(no)
			staticTruth(optCond) match {
				case Some(true) => if (pure(optCond)) optYes else Chain(Seq(optCond, optYes))
				case Some(false) => if (pure(optCond)) optNo else Chain(Seq(optCond, optNo))
				case None => Conditional(optCond, optYes, optNo)
			}

		case PropertyRead(receiver, property, safe) =>
			PropertyRead(optimize(receiver), optimize(property), safe)

		case PropertyWrite(receiver, property, value) =>
			PropertyWrite(optimize(receiver), optimize(property), optimize(value))

		case FunctionCall(target, args) =>
			FunctionCall(optimize(target), args.map(optimize))

		case Binary(op @ ("||" | "&&"), lhs, rhs) =>
			val optLhs = optimize(lhs)
			lazy val optRhs = optimize(rhs)
			(op, staticTruth(optLhs)) match {
				case ("||", Some(true)) => optLhs
				case ("||", Some(false)) if pure(optLhs) => optRhs
				case ("&&", Some(false)) => optLhs
				case ("&&", Some(true)) if pure(optLhs) => optRhs
				case _ => Binary(op, optLhs, optRhs)
			}

		case Binary(op, lhs, rhs) =>
			Binary(op, optimize(lhs), optimize(rhs))

		case Unary(op, operand) =>
			Unary(op, optimize(operand))

		case range: Range => staticValue(range) match {
			case Some(r) => LiteralPrimitive(r)
			case None => range
		}

		case StringView(expr) =>
			optimize(expr) match {
				case string @ LiteralPrimitive(_: String) => string
				case opt => StringView(opt)
			}

		case LiteralArray(values) => LiteralArray(values.map(optimize))
		case LiteralObject(values) => LiteralObject(values.map { case (key, value) => (optimize(key), optimize(value)) })

		case Interpolation(fragments) =>
			normalizeInterpolation(fragments.map {
				case f: StringFragment => f
				case ExpressionFragment(expr) =>
					val optimized = optimize(StringView(expr))
					precomputeFragment(optimized).getOrElse(ExpressionFragment(optimized))
			}.toList) match {
				case StringFragment(value) :: Nil => LiteralPrimitive(value)
				case ExpressionFragment(expr) :: Nil => expr
				case frags => Interpolation(frags)
			}

		case Enumerator(index, key, iterable, by, filter, locals) =>
			Enumerator(index, key, optimize(iterable), by.map(optimize), filter.map(optimize), locals.map(optimize))

		case _ => expression
	}
}
