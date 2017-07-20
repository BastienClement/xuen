package xuen.expression

trait ExpressionError {
	val msg: String
	override def toString: String = s"${getClass.getSimpleName}($msg)"
}

object ExpressionError {
	case class Lexer(msg: String) extends ExpressionError
	case class Parser(msg: String) extends ExpressionError
	case class Optimizer(msg: String) extends ExpressionError
	case class Interpreter(msg: String) extends ExpressionError
	case class Interpolation(msg: String) extends ExpressionError
}
