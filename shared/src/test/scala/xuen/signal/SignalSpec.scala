package xuen.signal

import xuen.BaseSpec
import xuen.signal.tools.MutationContext

class SignalSpec extends BaseSpec {
	"Signal constructor" - {
		"when given a constant" - {
			"should return a Constant" in {
				Signal(2) should be(a[Constant[_]])
			}
		}
		"when given an expression" - {
			"should return a Constant if the expression is constant" in {
				val x = Signal(2) // This is a constant
				Signal(x.value + 3) shouldBe Constant(5)
			}
			"should return an Expression if the expression is volatile" in {
				val x = Source(2) // This is mutable
				val y = Signal(x.value * 5)
				y shouldBe a[Expression[_]]
				y.value shouldBe 10
			}
			"should throw if the expression throws" in {
				an[NotImplementedError] should be thrownBy Signal(???)
			}
			"should produce an undefined signal if the expression throws UndefinedSignalException" - {
				"constant case" in {
					Signal(Signal.nil) should be theSameInstanceAs Signal.undefined
				}
				"direct mutable case" in {
					val x = Source.undefined
					val y = Signal(x.value)
					y shouldBe an[Expression[_]]
					y.isUndefined shouldBe true
				}
				"indirect mutable case" in {
					val x = Source(1)
					val y = Signal(x.value + (Signal.nil: Int))
					y shouldBe an[Expression[_]]
					y.isUndefined shouldBe true
				}
			}
		}
	}
	"Signal transformers" - {
		"map" - {
			"on undefined signal" - {
				val s = Signal.undefined
				"should not invoke the function" in (noException should be thrownBy s.map(_ => ???))
				"should return the undefined signal" in (s.map(identity) should be theSameInstanceAs s)
			}
			"on constant signal" - {
				val s = Signal(2)
				"should invoke the function eagerly" in (a[NotImplementedError] should be thrownBy s.map(_ => ???))
				"should return a constant signal" in (s.map(_ * 2) shouldBe Constant(4))
			}
			"on mutable signal" - {
				val s = Source(2)
				"should invoke the function eagerly" in (a[NotImplementedError] should be thrownBy s.map(_ => ???))
				"should return an expression signal" in {
					val t = s.map(_ * 2)
					t shouldBe an[Expression[_]]
					t.value shouldBe 4
					s := 3
					t.value shouldBe 6
				}
				"should handle Signal.nil" in {
					val t = s.map(_ => Signal.nil)
					t shouldBe an[Expression[_]]
					t.isUndefined shouldBe true
				}
			}
		}
		"flatMap" - {
			"on undefined signal" - {
				val s = Signal.undefined
				"should not invoke the function" in (noException should be thrownBy s.flatMap(_ => ???))
				"should return the undefined signal" in (s.flatMap(_ => ???) should be theSameInstanceAs s)
			}
			"on constant signal" - {
				val s = Signal(2)
				"should invoke the function eagerly" in (a[NotImplementedError] should be thrownBy s.flatMap(_ => ???))
				"should return a new constant signal if defined" in {
					val t = Constant(4)
					val u = s.flatMap(_ => t)
					u shouldBe Constant(4)
					u shouldNot be theSameInstanceAs t
				}
				"should return the undefined signal if undefined" in {
					s.flatMap(_ => Signal.undefined) should be theSameInstanceAs Signal.undefined
				}
			}
			"on mutable signal" - {
				val s = Source(2)
				"should invoke the function eagerly" in (a[NotImplementedError] should be thrownBy s.flatMap(_ => ???))
				"should return an expression signal" in {
					val t = s.flatMap(value => Signal(value * 2))
					t shouldBe an[Expression[_]]
					t.value shouldBe 4
					s := 3
					t.value shouldBe 6
				}
				"should handle Signal.nil" in {
					val t = s.flatMap(_ => Signal.nil)
					t shouldBe an[Expression[_]]
					t.isUndefined shouldBe true
				}
			}
		}
		"filter" - {
			"on undefined signal" - {
				val s = Signal.undefined
				"should not invoke the function" in (noException should be thrownBy s.filter(_ => ???))
				"should return the undefined signal" in (s.filter(_ => ???) should be theSameInstanceAs s)
			}
			"on constant signal" - {
				val s = Signal(2)
				"should invoke the function eagerly" in (a[NotImplementedError] should be thrownBy s.filter(_ => ???))
				"should return a new constant signal if predicate holds" in {
					val t = s.filter(_ > 0)
					t shouldBe Constant(2)
					t shouldNot be theSameInstanceAs s
				}
				"should return the undefined signal if predicate fails" in {
					s.filter(_ < 0) should be theSameInstanceAs Signal.undefined
				}
			}
			"on mutable signal" - {
				val s = Source(2)
				"should invoke the function eagerly" in (a[NotImplementedError] should be thrownBy s.filter(_ => ???))
				"should return an expression signal" - {
					"if the predicate holds" in (s.filter(_ > 0) shouldBe an[Expression[_]])
					"if the predicate fails" in (s.filter(_ < 0) shouldBe an[Expression[_]])
				}
				"should handle Signal.nil" in {
					val t = s.filter(_ => Signal.nil)
					t shouldBe an[Expression[_]]
					t.isUndefined shouldBe true
				}
			}
		}
		"fold" - {
			"on undefined signal" - {
				val s = Signal.undefined
				"should be a constant signal" in (s.fold(2)((_, _) => ???) shouldBe Constant(2))
			}
			"on constant signal" - {
				val s = Signal(2)
				"should be a constant signal" in (s.fold("foo")((acc, n) => acc * n) shouldBe Constant("foofoo"))
			}
			"on a mutable signal" - {
				val s = Source(2)
				"should behave correctly" in {
					val f = s.fold(3)((a, b) => a * b)
					f shouldBe an[Expression[_]]
					f.value shouldBe 6
					s := 3
					f.value shouldBe 18
					s := Source.nil
					f.value shouldBe 18
					s := 2
					f.value shouldBe 36
				}
			}
		}
		"reduce" - {
			"on undefined signal" - {
				val s = Signal.undefined
				"should be the undefined signal" in {
					s.reduce[Any]((_, _) => ???) should be theSameInstanceAs Signal.undefined
				}
			}
			"on constant signal" - {
				val s = Signal(2)
				"should be a constant signal" in {
					val t = s.reduce((_, _) => ???)
					t shouldBe Constant(2)
					t shouldNot be theSameInstanceAs s
				}
			}
			"on a mutable signal" - {
				val s = Source.undefined[Int]
				"should behave correctly" in {
					val f = s.reduce((a, b) => a * b)
					f shouldBe an[Expression[_]]
					f.isDefined shouldBe false
					s := 3
					f.value shouldBe 3
					s := Source.nil
					f.value shouldBe 3
					s := 2
					f.value shouldBe 6
				}
			}
		}
		"wrap" - {
			"should behave correctly" in {
				val s = Source(2)
				s.wrap.value should be theSameInstanceAs s.option
				val t = Source.undefined
				t.wrap.value shouldBe None
			}
		}
		"unwrap" - {
			"on undefined signal" - {
				val s = Signal.undefined
				"should behave correctly" in (s.unwrap.option shouldBe None)
			}
			"on constant signal" - {
				val s = Signal(None)
				val t = Signal(Some(2))
				"should behave correctly" in {
					s.unwrap should be theSameInstanceAs Signal.undefined
					t.unwrap.value shouldBe 2
				}
			}
			"on mutable signal" - {
				val s = Source(None)
				val t = Source(Some(2))
				"should behave correctly" in {
					val u = s.unwrap
					u shouldBe an[Expression[_]]
					u.option shouldBe None
					t.unwrap.value shouldBe 2
				}
			}
		}
		"flatten" - {
			"should behave correctly" in {
				val s = Signal(Signal(2))
				s.flatten shouldBe Constant(2)
			}
		}
	}
	"Source" - {
		"can be constructed" in {
			Source(2).isDefined shouldBe true
			Source.undefined.isDefined shouldBe false
			Source().isDefined shouldBe false
		}
		"can be assigned" in {
			val s = Source(2)
			s.option shouldBe Some(2)
			s := 3
			s.option shouldBe Some(3)
			s := Source.nil
			s.option shouldBe None
			s := 4
			s.option shouldBe Some(4)
		}
		"can be transformed" in {
			val s = Source(2)
			s.option shouldBe Some(2)
			s ~= (_ + 1)
			s.option shouldBe Some(3)
			s := Source.nil
			s ~= (_ + 1)
			s.option shouldBe None
		}
		"the ~= operator should not bind the invoker" in {
			val s = Source(1)
			val t = Source(0)
			val o = Observer(t ~= (_ + s.value))
			// The fact that nothing blew up there is already proof that the
			// operator is working as intended. If o was bound to t in its
			// definition, an infinite recursion would occur. Doing some more
			// tests just for fun.
			t.value shouldBe 1
			s := 2
			t.value shouldBe 3
			t := 0
			t.value shouldBe 0 // t := does not trigger the observer
		}
	}
	"Expression" - {
		"handle undefined parents" in {
			val s = Source(2)
			val t = Signal(s.value)
			t shouldBe an[Expression[_]]
			t.option shouldBe Some(2)
			s := Source.nil
			t.option shouldBe None
		}
		"cannot form circular dependency" in {
			// It is actually very hard to construct cycle between expression
			var a: Signal[Int] = null
			val b = Source(false)
			a = Signal(if (b.value) a.value / 2 else 0)
			b := true
			an[CircularDependencyException] should be thrownBy a.value
		}
	}
	"Mutation context" - {
		"can be nested" in {
			MutationContext.execute { outer =>
				MutationContext.execute { inner =>
					outer should be theSameInstanceAs inner
					2
				}
			} shouldBe 2
		}
		"can be used to build atomic mutation operations" in {
			val a = Source(1)
			val b = a.fold(0)(_ + _)
			b.value shouldBe 1
			Signal.atomically {
				a := 2
				a := 3
			}
			b.value shouldBe 4
		}
		"can catch a single exception" in {
			val s = Source(1)
			var a = 0
			val u = Observer({if (s.value == 0) ???; a += 1})
			val v = Observer({s.value; a += 2})
			// Both u and v are bound to the s source
			a shouldBe 3 // u + v were invoked once
			val e = the[MutationContext.MutationException] thrownBy (s := 0)
			e.count shouldBe 1
			e.causes should matchPattern { case List(_: NotImplementedError) => }
			a shouldBe 5 // the exception thrown by u should not have prevented v
		}
		"can catch multiple exceptions" in {
			val s = Source(1)
			val u = Observer(if (s.value == 0) ???)
			val v = Observer(if (s.value == 0) ???)
			val e = the[MutationContext.MutationException] thrownBy (s := 0)
			e.count shouldBe 2
			all(e.causes) shouldBe an[NotImplementedError]
		}
	}
	"Observers" - {
		"are evaluated eagerly" in {
			var a = 0
			Observer({a = 1})
			a shouldBe 1
		}
		"can be bound and unbound" in {
			val s = Source(1)
			var a = 0
			val o = Observer(a += s.value)
			a shouldBe 1
			// Calling twice to trigger the safety branch in code coverage
			o.unbind()
			o.unbind()
			s := 2
			a shouldBe 1
			o.bind()
			a shouldBe 3
			o.bind() // Only the first call is effective
			a shouldBe 3
			s := 5
			a shouldBe 8
		}
		"cannot mutate a parent signal" in {
			val s = Source(1)
			val o = Observer {
				// Cannot use ~= here since the operator is explicitly non-biding,
				// need to use `.value` explicitly
				s := s.value + 1
			}
			// Unable to detect illegal reference at definition-time, s is updated
			s.value shouldBe 2
			val e = the[MutationContext.MutationException] thrownBy (s := 0)
			e.count shouldBe 1
			e.causes.head shouldBe an[IllegalStateException]
			// A failing observer should not prevent the source from being mutated
			s.value shouldBe 0
		}
	}
}
