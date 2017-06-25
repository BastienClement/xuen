package xuen.expression
package parser

private[parser] sealed trait Token

private[parser] object Token {
	sealed trait AtomToken extends Token
	case class Identifier(name: String) extends AtomToken
	case class Selector(id: String) extends AtomToken
	case class StringLiteral(value: String) extends AtomToken
	case class NumberLiteral(value: Double) extends AtomToken

	sealed abstract class OperatorToken(val symbol: String) extends Token

	case object LeftParen extends OperatorToken("(")
	case object RightParen extends OperatorToken(")")
	case object LeftBrace extends OperatorToken("{")
	case object RightBrace extends OperatorToken("}")
	case object LeftBracket extends OperatorToken("[")
	case object RightBracket extends OperatorToken("]")

	case object Plus extends OperatorToken("+")
	case object Minus extends OperatorToken("-")
	case object Star extends OperatorToken("*")
	case object Slash extends OperatorToken("/")
	case object Percent extends OperatorToken("%")

	case object Lt extends OperatorToken("<")
	case object LtEq extends OperatorToken("<=")
	case object Gt extends OperatorToken(">")
	case object GtEq extends OperatorToken(">=")
	case object EqEq extends OperatorToken("==")
	case object NotEq extends OperatorToken("!=")
	case object EqEqEq extends OperatorToken("===")
	case object NotEqEq extends OperatorToken("!==")

	case object Question extends OperatorToken("?")
	case object Exclamation extends OperatorToken("!")

	case object And extends OperatorToken("&&")
	case object Or extends OperatorToken("||")

	case object BitAnd extends OperatorToken("&")
	case object BitOr extends OperatorToken("|")
	case object BitXor extends OperatorToken("^")

	case object Comma extends OperatorToken(",")
	case object Semicolon extends OperatorToken(";")
	case object Colon extends OperatorToken(":")
	case object LeftArrow extends OperatorToken("<-")
	case object RightArrow extends OperatorToken("->")
	case object Dot extends OperatorToken(".")
	case object SafeDot extends OperatorToken("?.")

	case object Equal extends OperatorToken("=")
	case object ColonEqual extends OperatorToken(":=")

	sealed abstract class KeywordToken(val literal: String) extends Token
	case object Val extends KeywordToken("val")
	case object Null extends KeywordToken("null")
	case object Undefined extends KeywordToken("undefined")
	case object True extends KeywordToken("true")
	case object False extends KeywordToken("false")
	case object If extends KeywordToken("if")
	case object Then extends KeywordToken("then")
	case object Else extends KeywordToken("else")
	case object Of extends KeywordToken("of")
	case object By extends KeywordToken("by")
	case object To extends KeywordToken("to")
}
