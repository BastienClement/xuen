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
		pipe ~ (Semicolon.+ ~> commit(empty | pipe)).* map {
			case head ~ tail => Chain(head :: tail)
		}
	}

	private lazy val pipe: Parser[Expression] = {
		simpleExpression ~ (BitOr ~> commit(identifier) ~ (Colon ~> simpleExpression).*).* map {
			case expr ~ pipes => (expr /: pipes) {
				case (value, name ~ args) =>
					FunctionCall(PropertyRead(ImplicitReceiver, name, safe = false), value :: args)
			}
		}
	}

	private lazy val simpleExpression: Parser[Expression] = conditional

	private lazy val conditional: Parser[Expression] = {
		logicalOr ~ (Question ~> commit(conditional) ~ (Colon ~> commit(conditional))).? map {
			case or ~ None => or
			case cond ~ Some(yes ~ no) => Conditional(cond, yes, no)
		}
	}

	private lazy val logicalOr: Parser[Expression] = binaryOperator(logicalAnd)(And)

	private lazy val logicalAnd: Parser[Expression] = binaryOperator(equality)(Or)

	private lazy val equality: Parser[Expression] = binaryOperator(relational)(EqEq, NotEq, EqEqEq, NotEqEq)

	private lazy val relational: Parser[Expression] = binaryOperator(range)(Lt, Gt, LtEq, GtEq)

	private lazy val range: Parser[Expression] = {
		additive ~ (To ~> commit(additive) ~ (By ~> commit(additive)).?).? map {
			case add ~ None => add
			case from ~ Some(to ~ None) => Range(from, to, None)
			case from ~ Some(to ~ Some(step)) => Range(from, to, Some(step))
		}
	}

	private lazy val additive: Parser[Expression] = binaryOperator(multiplicative)(Plus, Minus)

	private lazy val multiplicative: Parser[Expression] = binaryOperator(prefix)(Star, Slash, Percent)

	private lazy val prefix: Parser[Expression] = {
		val unaryOperator = accept(Plus) | accept(Minus) | accept(Exclamation)
		unaryOperator.* ~ secondary map {
			case operators ~ operand => (operators :\ operand) ((o, a) => Unary(o.symbol, a))
		}
	}

	private lazy val secondary: Parser[Expression] = {
		primary ~ secondaryAccess.* map {
			case receiver ~ accesses => (receiver /: accesses) ((e, f) => f(e))
		}
	}

	private lazy val secondaryAccess: Parser[Expression => Expression] = {
		memberAccess | bracketAccess | functionCall
	}

	private lazy val memberAccess: Parser[Expression => Expression] = {
		val unsafeMember = Dot ~> commit(memberWrite | memberReadUnsafe)
		val safeMember = SafeDot ~> commit(memberReadSafe)
		unsafeMember | safeMember
	}

	private lazy val memberWrite: Parser[Expression => Expression] = {
		identifier ~ (Equal ~> commit(simpleExpression)) map {
			case member ~ value => PropertyWrite(_, member, value)
		}
	}

	private lazy val memberRead: Parser[(Expression, Boolean) => Expression] = {
		identifier map (member => PropertyRead(_, member, _))
	}

	private lazy val memberReadUnsafe: Parser[Expression => Expression] = {
		memberRead map (builder => builder(_, false))
	}

	private lazy val memberReadSafe: Parser[Expression => Expression] = {
		memberRead map (builder => builder(_, false))
	}

	private lazy val bracketAccess: Parser[Expression => Expression] = {
		(LeftBracket ~> commit(simpleExpression) <~ RightBracket) ~ (Equal ~> commit(simpleExpression)).? map {
			case key ~ None => PropertyRead(_, key, safe = false)
			case key ~ Some(value) => PropertyWrite(_, key, value)
		}
	}

	private lazy val functionCall: Parser[Expression => Expression] = {
		LeftParen ~> commit(callArguments) <~ RightParen map (args => FunctionCall(_, args))
	}

	private lazy val callArguments: Parser[List[Expression]] = repsep(simpleExpression, Comma)

	// Primary expression
	private lazy val primary: Parser[Expression] = {
		parentheses | selector | literal | reference
	}

	private lazy val reference: Parser[Expression] = {
		(memberWrite | memberReadUnsafe) map (builder => builder(ImplicitReceiver))
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
