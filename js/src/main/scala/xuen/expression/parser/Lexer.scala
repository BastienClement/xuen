package xuen.expression.parser

import scala.util.parsing.combinator.RegexParsers
import xuen.expression.parser.Token._

object Lexer extends RegexParsers {
	override def skipWhitespace: Boolean = true

	/** Parses operators */
	private def operator: Parser[OperatorToken] = Seq(
		LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
		Plus, Minus, Star, Slash, Percent,
		Lt, LtEq, Gt, GtEq, EqEq, NotEq, EqEqEq, NotEqEq,
		And, Or,
		BitAnd, BitOr, BitXor,
		Comma, Semicolon, Colon, LeftArrow, RightArrow, Dot, SafeDot,
		Equal, ColonEqual
	).sortBy(op => -op.symbol.length).map(op => op.symbol ^^ { _ => op }).reduce(_ | _)

	/** Parses keywords */
	private def keyword: Parser[KeywordToken] = Seq(
		Val, Null, Undefined, True, False, If, Then, Else, Of, By, To
	).sortBy(kw => -kw.literal.length).map(kw => (kw.literal + "\\b").r ^^ { _ => kw }).reduce(_ | _)

	/** Parses an identifier */
	private def identifier: Parser[Identifier] = "[a-zA-Z_$][a-zA-Z0-9_$]*".r ^^ Identifier

	/** Parses a selector */
	private def selector: Parser[Selector] = "#[a-zA-Z0-9\\-]+".r ^^ Selector

	// Pattern matching a string literal
	private val stringPattern = """(?=["'])(?:"[^"\\]*(?:\\[\s\S][^"\\]*)*"|'[^'\\]*(?:\\[\s\S][^'\\]*)*')""".r

	/** Parses a string literal */
	private def string: Parser[StringLiteral] = {
		stringPattern ^^ { literal =>
			// Handle character escape sequences
			StringLiteral("\\\\(.)".r.replaceAllIn(literal.drop(1).dropRight(1), { m =>
				m.group(1) match {
					case "b" => "\b"
					case "f" => "\f"
					case "n" => "\n"
					case "r" => "\r"
					case "t" => "\t"
					case "\\" => "\\\\"
					case other => other
				}
			}))
		}
	}

	// http://www.ecma-international.org/ecma-262/6.0/#sec-literals-numeric-literals (11.8.3 Numeric Literals):
	// The SourceCharacter immediately following a NumericLiteral must not be an IdentifierStart or DecimalDigit.
	private val numberPattern = """((0|[1-9][0-9]*)(\.[0-9]+)?|\.[0-9]+)([eE][+\-]?[0-9]+)?(?![a-zA-Z_$]|[0-9])""".r

	private def decNumber: Parser[NumberLiteral] = numberPattern ^^ java.lang.Double.parseDouble ^^ NumberLiteral

	private def integer(pattern: String, base: Int): Parser[NumberLiteral] = {
		(pattern + "(?![a-zA-Z_$]|[0-9])").r ^^ { literal =>
			NumberLiteral(Integer.parseInt(literal.drop(2), base).toDouble)
		}
	}

	private def hexInteger: Parser[NumberLiteral] = integer("0[xX][0-9a-fA-F]+", 16)
	private def octInteger: Parser[NumberLiteral] = integer("0[oO][0-7]+", 8)
	private def binInteger: Parser[NumberLiteral] = integer("0[bB][01]+", 2)

	/** Parses a number literal */
	private def number: Parser[NumberLiteral] = hexInteger | octInteger | binInteger | decNumber

	/** Parses a sequence of tokens */
	private def tokens: Parser[List[Token]] = {
		phrase(rep1(keyword | number | string | identifier | selector | operator))
	}

	/** Lexer error */
	case class LexerError(msg: String)

	/** Tokenize the given input into a list of tokens */
	def apply(input: String): Either[LexerError, List[Token]] = {
		parse(tokens, input) match {
			case NoSuccess(msg, _) => Left(LexerError(msg))
			case Success(result, _) => Right(result)
		}
	}
}
