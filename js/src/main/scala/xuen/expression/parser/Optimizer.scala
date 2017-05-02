package xuen.expression.parser

import scala.scalajs.js
import scala.scalajs.js.DynamicImplicits.truthValue
import xuen.expression.Expression
import xuen.expression.Expression._

private[expression] object Optimizer {
	/** Attempts to find known compile time value */
	private def staticValue(expression: Expression): Option[Any] = expression match {
		case Empty =>
			Some(js.undefined)

		case Chain(exprs) =>
			if (exprs.isEmpty) Some(scalajs.js.undefined) else staticValue(exprs.last)

		case Conditional(cond, yes, no) =>
			staticTruth(cond) match {
				case Some(true) => staticValue(yes)
				case Some(false) => staticValue(no)
				case None => for (y <- staticValue(yes); n <- staticValue(no); if y == n) yield y
			}

		case PropertyWrite(_, _, value) => staticValue(value)
		case LiteralPrimitive(value) => Some(value)

		case Range(from, to, step) =>
			(staticValue(from), staticValue(to), step.flatMap(staticValue)) match {
				case (Some(a: Int), Some(b: Int), Some(c: Int)) => Some(a to b by c)
				case (Some(a: Int), Some(b: Int), None) => Some(a to b)
				case _ => None
			}

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

		case LiteralArray(_) | LiteralObject(_) => Some(true)

		case _ => staticValue(expression).map(_.asInstanceOf[js.Dynamic]: Boolean)
	}

	/* An expression is pure if it does not have any visible side effects */
	private def pure(expression: Expression): Boolean = expression match {
		case Empty => true
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

		case PropertyRead(_, _, _) | MethodCall(_, _, _, _) | PropertyWrite(_, _, _) |
		     FunctionCall(_, _) | Reactive(_) => false

		case _ => throw new IllegalArgumentException(s"Cannot infer pureness from $expression")
	}

	/** Executes a binary operation at compile time */
	private def staticBinary(op: String, lhs: js.Dynamic, rhs: js.Dynamic): Option[Any] = Some(op match {
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
		case _ => return None
	})

	/** Executes a unary operation at compile time */
	private def staticUnary(op: String, operand: js.Dynamic): Option[Any] = Some(op match {
		case "+" => operand.unary_+()
		case "-" => operand.unary_-()
		case "!" => operand.unary_!()
		case _ => return None
	})

	/** Joins sequential StringFragments in the interpolation */
	private def flattenInterpolation(fragments: List[InterpolationFragment]): List[InterpolationFragment] = fragments match {
		case Nil | (_ :: Nil) => fragments
		case StringFragment(a) :: StringFragment(b) :: tail => flattenInterpolation(StringFragment(a + b) :: tail)
		case head :: tail => head :: flattenInterpolation(tail)
	}

	/** Optimize the given expression */
	def optimize(expression: Expression): Expression = expression match {
		case Chain(expressions) if expressions.length < 1 =>
			Empty

		case Chain(expressions) =>
			val optimized = expressions.map(optimize)
			val filtered = optimized.dropRight(1).filter(!pure(_)) :+ optimized.last
			if (filtered.length == 1) filtered.head
			else Chain(filtered)

		case Conditional(cond, yes, no) =>
			val optCond = optimize(cond)
			lazy val optYes = optimize(yes)
			lazy val optNo = optimize(no)
			staticTruth(optCond) match {
				case Some(true) => if (pure(optCond)) optYes else Chain(Seq(optCond, optYes))
				case Some(false) => if (pure(optCond)) optNo else Chain(Seq(optCond, optNo))
				case None => Conditional(optCond, optYes, optNo)
			}

		case PropertyRead(receiver, property, safe) => PropertyRead(optimize(receiver), optimize(property), safe)
		case PropertyWrite(receiver, property, value) => PropertyWrite(optimize(receiver), optimize(property), optimize(value))
		case MethodCall(receiver, method, args, safe) => MethodCall(optimize(receiver), optimize(method), args.map(optimize), safe)
		case FunctionCall(target, args) => FunctionCall(optimize(target), args.map(optimize))

		case Binary(op, lhs, rhs) =>
			val optLhs = optimize(lhs)
			lazy val optRhs = optimize(rhs)
			(op, staticTruth(optLhs)) match {
				case ("||", Some(true)) => optLhs
				case ("||", Some(false)) if pure(optLhs) => optRhs
				case ("&&", Some(false)) => optLhs
				case ("&&", Some(true)) if pure(optLhs) => optRhs
				case _ =>
					(for {
						left <- staticValue(optLhs)
						right <- staticValue(optRhs)
						value <- staticBinary(op, left.asInstanceOf[js.Dynamic], right.asInstanceOf[js.Dynamic])
					} yield LiteralPrimitive(value)).getOrElse(Binary(op, optLhs, optRhs))
			}

		case Unary(op, operand) =>
			val optOperand = optimize(operand)
			(for {
				value <- staticValue(operand)
				result <- staticUnary(op, value.asInstanceOf[js.Dynamic])
			} yield LiteralPrimitive(result)).getOrElse(Unary(op, optOperand))

		case range: Range => staticValue(range) match {
			case Some(r) => LiteralPrimitive(r)
			case None => range
		}

		case LiteralArray(values) => LiteralArray(values.map(optimize))
		case LiteralObject(values) => LiteralObject(values.map { case (key, value) => (optimize(key), optimize(value)) })

		case Interpolation(fragments) =>
			flattenInterpolation(fragments.map {
				case f: StringFragment => f
				case ExpressionFragment(expr) =>
					val optExpr = optimize(expr)
					staticValue(optExpr) match {
						case Some(value) if pure(optExpr) => StringFragment(value.toString)
						case _ => ExpressionFragment(optExpr)
					}
			}.toList) match {
				case StringFragment(value) :: Nil => LiteralPrimitive(value)
				case ExpressionFragment(expr) :: Nil => expr
				case frags => Interpolation(frags)
			}

		case Enumerator(index, key, iterable, by, filter, locals) =>
			Enumerator(index, key, optimize(iterable), by.map(optimize), filter.map(optimize), locals.map(optimize))

		case Reactive(expr) => Reactive(optimize(expr))

		case _ => expression
	}
}
