package xuen.expression

import xuen.expression.parser.{Lexer, Parser}

sealed trait Expression {

}

object Expression {
	case object Empty extends Expression
	case object ImplicitReceiver extends Expression

	case class Chain(parts: Seq[Expression]) extends Expression
	case class Conditional(cond: Expression, yes: Expression, no: Expression) extends Expression

	case class PropertyRead(receiver: Expression, property: Expression, safe: Boolean) extends Expression
	case class MethodCall(receiver: Expression, method: Expression, args: Seq[Expression], safe: Boolean) extends Expression
	case class PropertyWrite(receiver: Expression, property: Expression, value: Expression) extends Expression

	case class FunctionCall(target: Expression, args: Seq[Expression]) extends Expression

	case class Binary(op: String, lhs: Expression, rhs: Expression) extends Expression
	case class Unary(op: String, operand: Expression) extends Expression

	case class Range(from: Expression, to: Expression, step: Option[Expression]) extends Expression
	case class SelectorQuery(selector: String) extends Expression

	case class LiteralPrimitive(value: Any) extends Expression
	case class LiteralArray(values: Seq[Expression]) extends Expression
	case class LiteralObject(values: Seq[(Expression, Expression)]) extends Expression

	case class Interpolation(fragments: Seq[InterpolationFragment]) extends Expression

	sealed trait InterpolationFragment
	case class StringFragment(value: String) extends InterpolationFragment
	case class ExpressionFragment(expression: Expression) extends InterpolationFragment

	case class Enumerator(index: Option[String], key: String, iterable: Expression, by: Option[Expression],
	                      filter: Option[Expression], locals: Option[Expression]) extends Expression {
		val indexKey: String = index.getOrElse("$key")
	}

	case class Reactive(expression: Expression) extends Expression

	def parse(input: String): Either[ExpressionError, Expression] = {
		Lexer(input).flatMap(Parser(_, Parser.expression))
	}

	def parseEnumerator(input: String): Either[ExpressionError, Enumerator] = {
		Lexer(input).flatMap(Parser(_, Parser.enumerator)).map(_.asInstanceOf[Enumerator])
	}
}
