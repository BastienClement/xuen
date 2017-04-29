package xuen.expression.parser

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
	def first: Token = tokens.head
	def rest: Reader[Token] = new TokenReader(tokens.tail)
	def pos: Position = NoPosition
	def atEnd: Boolean = tokens.isEmpty
}
