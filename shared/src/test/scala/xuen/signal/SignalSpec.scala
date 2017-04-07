package xuen.signal

import xuen.BaseSpec

class SignalSpec extends BaseSpec {
	"A signal" when {
		"undefined" should {
			val undef = Signal.undefined
			"be undefined" in {
				undef.isDefined shouldBe false
				undef.isUndefined shouldBe true
			}
			"have no-ops monadic operators" in {
				undef.map(_ => ???) should be theSameInstanceAs undef
				undef.flatMap(_ => ???) should be theSameInstanceAs undef
				undef.filter(_ => ???) should be theSameInstanceAs undef
			}
		}

		"constant" should {
			val const = Signal.wrap(2)
			"be defined" in {
				const.isDefined shouldBe true
				const.isUndefined shouldBe false
			}
			"have eager monadic operators" in {
				const.map(_ * 2) should matchPattern { case Constant(4) => }
				val other = Signal.wrap(0)
				const.flatMap(_ => other) should be theSameInstanceAs other
				const.filter(_ > 0) should be theSameInstanceAs const
				const.filter(_ < 0) should be theSameInstanceAs Signal.undefined
			}
		}
	}
}
