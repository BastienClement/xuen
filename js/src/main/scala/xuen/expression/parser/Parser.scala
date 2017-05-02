package xuen.expression.parser

import scala.language.implicitConversions
import scala.scalajs.js
import scala.util.parsing.combinator.Parsers
import xuen.expression.Expression._
import xuen.expression.parser.Token._
import xuen.expression.{Expression, ExpressionParserError}

private[expression] object Parser extends Parsers {
	override type Elem = Token

	private implicit def accept[T <: Token](e: T): Parser[T] = Parser { in =>
		if (in.atEnd) Failure("end of input", in)
		else if (in.first == e) Success(e, in.rest)
		else Failure("`" + e + "' expected but " + in.first + " found", in)
	}

	def apply(tokens: Seq[Token], mode: Parser[Expression]): Either[ExpressionParserError, Expression] = {
		mode(new TokenReader(tokens)) match {
			case NoSuccess(msg, _) => Left(ExpressionParserError(msg))
			case Success(result, _) => Right(Optimizer.optimize(result))
		}
	}

	// Identifier token
	private lazy val identifier: Parser[Expression] = {
		accept("identifier", { case i: Identifier => LiteralPrimitive(i.name) })
	}

	lazy val expression: Parser[Expression] = phrase(empty | chain)

	lazy val enumerator: Parser[Expression] = ???

	private lazy val empty: Parser[Expression] = { in =>
		if (in.atEnd) Success(Empty, in)
		else Failure("", in)
	}

	private lazy val chain: Parser[Expression] = {
		pipe ~ (Semicolon.+ ~> (empty | pipe)).* map {
			case head ~ tail => Chain(head :: tail)
		}
	}

	private lazy val pipe: Parser[Expression] = {
		simpleExpression ~ (BitOr ~! identifier ~ (Colon ~> simpleExpression).*).* map {
			case expr ~ pipes => (expr /: pipes) {
				case (value, _ ~ name ~ args) =>
					MethodCall(ImplicitReceiver, name, value :: args, safe = false)
			}
		}
	}

	private lazy val simpleExpression: Parser[Expression] = conditional

	private lazy val conditional: Parser[Expression] = {
		logicalOr ~ (Question ~! conditional ~! Colon ~! conditional).? map {
			case or ~ None => or
			case cond ~ Some(_ ~ yes ~ _ ~ no) => Conditional(cond, yes, no)
		}
	}

	private lazy val logicalOr: Parser[Expression] = binaryOperator(logicalAnd)(And)

	private lazy val logicalAnd: Parser[Expression] = binaryOperator(equality)(Or)

	private lazy val equality: Parser[Expression] = binaryOperator(relational)(EqEq, NotEq, EqEqEq, NotEqEq)

	private lazy val relational: Parser[Expression] = binaryOperator(range)(Lt, Gt, LtEq, GtEq)

	private lazy val range: Parser[Expression] = {
		additive ~ (To ~! additive ~ (By ~! additive).?).? map {
			case add ~ None => add
			case from ~ Some(_ ~ to ~ None) => Range(from, to, None)
			case from ~ Some(_ ~ to ~ Some(_ ~ step)) => Range(from, to, Some(step))
		}
	}

	private lazy val additive: Parser[Expression] = binaryOperator(multiplicative)(Plus, Minus)

	private lazy val multiplicative: Parser[Expression] = binaryOperator(prefix)(Star, Slash, Percent)

	private lazy val prefix: Parser[Expression] = {
		val unaryOperator = accept(Plus) | accept(Minus) | accept(Exclamation)
		unaryOperator.* ~ callChain map {
			case operators ~ operand => (operators :\ operand)((o, a) => Unary(o.symbol, a))
		}
	}

	private lazy val callChain: Parser[Expression] = primary

	// Primary expression
	private lazy val primary: Parser[Expression] = {
		parentheses | selector | literal
	}

	private lazy val parentheses: Parser[Expression] = {
		LeftParen ~> chain <~ RightParen
	}

	// Selector
	private lazy val selector: Parser[Expression] = {
		accept("selector", { case s: Selector => SelectorQuery(s.id) })
	}

	private lazy val literal: Parser[Expression] = {
		keywordLiteral | numberLiteral | stringLiteral | arrayLiteral | objectLiteral
	}

	private lazy val keywordLiteral: Parser[Expression] = {
		undefinedLiteral | nullLiteral | falseLiteral | trueLiteral
	}

	private lazy val undefinedLiteral: Parser[Expression] = Undefined ^^^ LiteralPrimitive(js.undefined)

	private lazy val nullLiteral: Parser[Expression] = Null ^^^ LiteralPrimitive(null)

	private lazy val falseLiteral: Parser[Expression] = False ^^^ LiteralPrimitive(false)

	private lazy val trueLiteral: Parser[Expression] = True ^^^ LiteralPrimitive(true)

	private lazy val numberLiteral: Parser[Expression] = {
		accept("number literal", { case n: NumberLiteral => LiteralPrimitive(n.value) })
	}

	private lazy val stringLiteral: Parser[Expression] = {
		accept("string literal", { case s: StringLiteral => LiteralPrimitive(s.value) })
	}

	private lazy val arrayLiteral: Parser[Expression] = {
		LeftBracket ~> repsep(simpleExpression, Comma) <~ RightBracket map LiteralArray
	}

	private lazy val dynamicIndex: Parser[Expression] = {
		LeftBracket ~> simpleExpression <~ RightBracket
	}

	private lazy val objectLiteral: Parser[Expression] = {
		val key = identifier | stringLiteral | dynamicIndex
		val entry = key ~ (Colon ~> simpleExpression) map { case k ~ v => (k, v) }
		LeftBrace ~> repsep(entry, Comma) <~ RightBrace map LiteralObject
	}

	private def binaryOperator(next: Parser[Expression])
	                          (op: OperatorToken, ops: OperatorToken*): Parser[Expression] = {
		val operators = (accept(op) /: ops) (_ | _)
		next ~ (operators ~! next).* map {
			case first ~ rest => (first /: rest) {
				case (lhs, operator ~ rhs) => Binary(operator.symbol, lhs, rhs)
			}
		}
	}
}
