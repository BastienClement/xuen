package xuen.signal

import org.scalatest.{FlatSpec, Matchers}

class SignalSpec extends FlatSpec with Matchers {
	"An empty signal" should "be empty" in {
		val s = Signal.empty
		s.isDefined shouldBe false
		s.isEmpty shouldBe true
	}

	it should "not be lazy" in {
		val s = Signal.empty
		s shouldBe Empty
		s.map(identity) shouldBe s
		s.flatMap(identity) shouldBe s
		s.filter(_ => true) shouldBe s
	}
}
