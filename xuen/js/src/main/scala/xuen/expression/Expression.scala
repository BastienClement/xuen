package xuen.expression

import scala.annotation.tailrec
import xuen.expression.parser.{Lexer, Optimizer, Parser}

sealed trait Expression

object Expression {
	case object Empty extends Expression
	case object ImplicitReceiver extends Expression

	case class Chain(parts: Seq[Expression]) extends Expression
	case class Conditional(cond: Expression, yes: Expression, no: Expression) extends Expression

	case class PropertyRead(receiver: Expression, property: Expression, safe: Boolean) extends Expression
	case class PropertyWrite(receiver: Expression, property: Expression, value: Expression) extends Expression

	case class FunctionCall(target: Expression, args: Seq[Expression]) extends Expression

	case class Binary(op: String, lhs: Expression, rhs: Expression) extends Expression
	case class Unary(op: String, operand: Expression) extends Expression

	case class Range(from: Expression, to: Expression, step: Option[Expression]) extends Expression
	case class SelectorQuery(selector: String) extends Expression
	case class StringView(expression: Expression) extends Expression

	case class LiteralPrimitive(value: Any) extends Expression
	case class LiteralArray(values: Seq[Expression]) extends Expression
	case class LiteralObject(values: Seq[(Expression, Expression)]) extends Expression

	case class Interpolation(fragments: Seq[InterpolationFragment]) extends Expression

	sealed trait InterpolationFragment
	case class StringFragment(value: String) extends InterpolationFragment
	case class ExpressionFragment(expression: Expression) extends InterpolationFragment

	case class Enumerator(key: Option[String], value: String, iterable: Expression, by: Option[Expression],
	                      filter: Option[Expression], locals: Option[Expression]) extends Expression

	/**
	  * Parses and optimizes a template expression.
	  *
	  * @param input the input expression
	  */
	def parse(input: String): Either[ExpressionError, Expression] = {
		Lexer(input).flatMap(Parser(_, Parser.expression)).map(Optimizer.optimize)
	}

	/**
	  * Parses an enumerator expression.
	  *
	  * @param input the input expression
	  */
	def parseEnumerator(input: String): Either[ExpressionError, Enumerator] = {
		Lexer(input).flatMap(Parser(_, Parser.enumerator)).map(Optimizer.optimize).map(_.asInstanceOf[Enumerator])
	}

	/**
	  * Parses an interpolation expression.
	  *
	  * An interpolation is a mix of string literals and template expressions
	  * enclosed in {{ and }} tags.
	  *
	  * This method will return None if no interpolation are found in the
	  * given input. Event if this method returns an expression, this expression
	  * is not guaranteed to be an [[Interpolation]].
	  *
	  * If the full interpolation can be computed at compile time, this method
	  * will return a single [[LiteralPrimitive]] node wrapping the resulting
	  * string. In this case, the caller should replace the interpolation by
	  * this value and avoid calling the interpreter.
	  *
	  * If the interpolation cannot be computed at compile time but is composed
	  * of a single expression, this expression node is returned without being
	  * wrapped in an [[Interpolation]] node. In this case, the expression may
	  * be wrapped in a [[StringView]] node. Evaluating such an expression will
	  * always return a string value and handle null values safely.
	  *
	  * If the interpolation cannot be optimized away, an [[Interpolation]] will
	  * be returned and will required evaluation at run-time by the interpreter.
	  * Such node is also guaranteed to evaluate to a string value.
	  *
	  * @param input the input expression
	  */
	def parseInterpolation(input: String): Either[ExpressionError, Option[Expression]] = {
		// Fast shortcut if no interpolation
		if (input.indexOf("{{") < 0) Right(None)
		else {
			type Fragments = List[InterpolationFragment]
			@tailrec
			def parseNext(from: Int, fragments: Fragments = Nil): Either[ExpressionError, Fragments] = {
				val begin = input.indexOf("{{", from)
				if (begin < 0) {
					if (from < input.length) Right((StringFragment(input.substring(from)) :: fragments).reverse)
					else Right(fragments.reverse)
				} else {
					if (begin != from) parseNext(begin, StringFragment(input.substring(from, begin)) :: fragments)
					else {
						val end = input.indexOf("}}", from)
						if (end < 0) {
							Left(ExpressionError.Interpolation(s"Unterminated interpolation expression in '${input.substring(begin)}'"))
						} else {
							parse(input.substring(begin + 2, end)) map ExpressionFragment match {
								case Left(err) => Left(err)
								case Right(fragment) => parseNext(end + 2, fragment :: fragments)
							}
						}
					}
				}
			}
			parseNext(0) map { fragments => Some(Optimizer.optimize(Interpolation(fragments))) }
		}
	}
}
