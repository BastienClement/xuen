package xuen.expression.parser

import scala.util.parsing.combinator.PackratParsers
import xuen.expression.Expression
import xuen.expression.parser.Token._

object Parser extends PackratParsers {
	override type Elem = Token

	private lazy val identifier: PackratParser[Identifier] = accept("identifier", { case i: Identifier => i })
	private lazy val selector: PackratParser[Selector] = accept("selector", { case s: Selector => s })
	private lazy val stringLiteral: PackratParser[StringLiteral] = accept("string literal", { case s: StringLiteral => s })
	private lazy val numberLiteral: PackratParser[NumberLiteral] = accept("number literal", { case n: NumberLiteral => n })

	def expression: Parser[Expression] = ???
	def enumerator: Parser[Expression] = ???

	def apply(tokens: Seq[Token], mode: Parser[Expression]): Either[ExpressionParseError, Expression] = {
		mode(new TokenReader(tokens)) match {
			case NoSuccess(msg, _) => Left(ExpressionParseError(msg))
			case Success(result, _) => Right(Optimizer.optimize(result))
		}
	}
}
