package xuen.expression

trait ExpressionError

case class ExpressionLexerError(msg: String) extends ExpressionError
case class ExpressionParserError(msg: String) extends ExpressionError
case class ExpressionOptimizerError(msg: String) extends ExpressionError
case class ExpressionInterpreterError(msg: String) extends ExpressionError
