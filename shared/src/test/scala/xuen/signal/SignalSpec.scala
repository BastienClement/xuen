package xuen.signal

import org.scalatest.{FlatSpec, Matchers}

class SignalSpec extends FlatSpec with Matchers {
	"An empty signal" should "be empty" in {
		val s = Signal.undefined
		s.isDefined shouldBe false
		s.isUndefined shouldBe true
	}

	it should "not be lazy" in {
		val s = Signal.undefined
		s shouldBe Undefined
		s.map(identity) shouldBe s
		s.flatMap(identity) shouldBe s
		s.filter(_ => true) shouldBe s
	}

	"A stable signal" should "hold its value" in {
		val source = Source[Int]()
		val s = Signal.stable(source)
		s.isUndefined shouldBe true
		source := 2
		s.isDefined shouldBe true
		s.value shouldBe 2
		source := UndefinedValue
		s.isDefined shouldBe true
		s.value shouldBe 2
		source := 4
		s.value shouldBe 4

		val test: Signal[Int] = Some(2)
		val test2: Signal[Int] = None
	}
}
