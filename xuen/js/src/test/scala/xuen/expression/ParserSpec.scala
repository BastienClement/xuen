package xuen.expression

import scala.scalajs.js
import xuen.BaseSpec
import xuen.expression.Expression._
import xuen.expression.parser.{Lexer, Optimizer, Parser}

class ParserSpec extends BaseSpec {
	implicit class ExpressionMatcher (input: String) {
		def ~> (e: Expression)(implicit mode: Parser.Parser[Expression]): Unit = {
			Lexer(input).flatMap(Parser(_, mode)) shouldBe Right(e)
		}
		def ~!> (e: Expression)(implicit mode: Parser.Parser[Expression]): Unit = {
			Lexer(input).flatMap(Parser(_, mode)).map(Optimizer.optimize) shouldBe Right(e)
		}
	}

	"Expressions" - {
		implicit val mode = Parser.expression
		"Primary" - {
			"Parentheses" in {
				"(1 + 2) + (3 + 4)" ~> Chain(Seq(
					Binary(
						"+",
						Chain(Seq(
							Binary("+", LiteralPrimitive(1), LiteralPrimitive(2))
						)),
						Chain(Seq(
							Binary("+", LiteralPrimitive(3), LiteralPrimitive(4))
						))
					)
				))
			}
			"Selector" - {
				"Simple" in ("#test" ~!> SelectorQuery("#test"))
				"Complex" in ("@(.cls)" ~!> SelectorQuery(".cls"))
			}
			"Reference" - {
				"Read" in {
					"a" ~!> PropertyRead(ImplicitReceiver, LiteralPrimitive("a"), safe = false)
				}
				"Write" in {
					"a = 1" ~!> PropertyWrite(ImplicitReceiver, LiteralPrimitive("a"), LiteralPrimitive(1))
				}
			}
			"Literals" - {
				"Undefined" in {
					"undefined" ~!> LiteralPrimitive(js.undefined)
				}
				"Null" in {
					"null" ~!> LiteralPrimitive(null)
				}
				"Boolean" in {
					"true" ~!> LiteralPrimitive(true)
					"false" ~!> LiteralPrimitive(false)
				}
				"Number" in {
					"2" ~!> LiteralPrimitive(2)
					"3.14" ~!> LiteralPrimitive(3.14)
				}
				"String" in {
					""" "foo" """ ~!> LiteralPrimitive("foo")
					"'bar'" ~!> LiteralPrimitive("bar")
				}
				"Array" in {
					"[1, 2]" ~> Chain(Seq(
						LiteralArray(Seq(
							LiteralPrimitive(1),
							LiteralPrimitive(2)
						))
					))
				}
				"Object" in {
					"{ a: 1, b: 2, [a]: null, 'b': false }" ~> Chain(Seq(
						LiteralObject(Seq(
							LiteralPrimitive("a") -> LiteralPrimitive(1),
							LiteralPrimitive("b") -> LiteralPrimitive(2),
							PropertyRead(ImplicitReceiver, LiteralPrimitive("a"), safe = false) -> LiteralPrimitive(null),
							LiteralPrimitive("b") -> LiteralPrimitive(false)
						))
					))
				}
			}
		}
		"Function call" - {
			"Global" in {
				"foo(1, 2)" ~!> FunctionCall(
					PropertyRead(ImplicitReceiver, LiteralPrimitive("foo"), safe = false),
					Seq(LiteralPrimitive(1), LiteralPrimitive(2))
				)
			}
			"Method" in {
				"foo?.bar(1, 2)" ~!> FunctionCall(
					PropertyRead(
						PropertyRead(ImplicitReceiver, LiteralPrimitive("foo"), safe = false),
						LiteralPrimitive("bar"),
						safe = true
					),
					Seq(LiteralPrimitive(1), LiteralPrimitive(2))
				)
			}
		}
	}

	"Enumerators" - {
		implicit val mode = Parser.enumerator
		"Simple" in ("foo of null" ~> Enumerator(None, "foo", LiteralPrimitive(null), None, None, None))
		"Double" in ("foo, bar of null" ~> Enumerator(Some("foo"), "bar", LiteralPrimitive(null), None, None, None))
		"Complete" in {
			"foo, bar of iterable by bar.key if foo < 3 with a = 2; b = 3" ~!> Enumerator(
				Some("foo"),
				"bar",
				PropertyRead(ImplicitReceiver, LiteralPrimitive("iterable"), safe = false),
				Some(PropertyRead(
					PropertyRead(ImplicitReceiver, LiteralPrimitive("bar"), safe = false),
					LiteralPrimitive("key"),
					safe = false
				)),
				Some(Binary(
					"<",
					PropertyRead(ImplicitReceiver, LiteralPrimitive("foo"), safe = false),
					LiteralPrimitive(3)
				)),
				Some(Chain(Seq(
					PropertyWrite(ImplicitReceiver, LiteralPrimitive("a"), LiteralPrimitive(2)),
					PropertyWrite(ImplicitReceiver, LiteralPrimitive("b"), LiteralPrimitive(3))
				)))
			)
		}
	}

	"Interpolations" - {
		"None" in (Expression.parseInterpolation("hello") shouldBe Right(None))
		"Simple" in (Expression.parseInterpolation("{{'foo'}}") shouldBe Right(Some(LiteralPrimitive("foo"))))
		"Standard" in {
			Expression.parseInterpolation("foo {{bar()}} baz") shouldBe Right(Some(
				Interpolation(Seq(
					StringFragment("foo "),
					ExpressionFragment(StringView(
						FunctionCall(
							PropertyRead(ImplicitReceiver, LiteralPrimitive("bar"), safe = false),
							Seq()
						)
					)),
					StringFragment(" baz")
				))
			))
		}
		"Optimized" in {
			Expression.parseInterpolation("{{bar()}}") shouldBe Right(Some(
				StringView(
					FunctionCall(
						PropertyRead(ImplicitReceiver, LiteralPrimitive("bar"), safe = false),
						Seq()
					)
				)
			))
		}
		"Pre-computed" in {
			Expression.parseInterpolation("foo {{2 + 3}} baz") shouldBe Right(Some(
				LiteralPrimitive("foo 5 baz")
			))
		}
	}
}
