package xuen.component

import java.lang.{Byte => JByte, Double => JDouble, Float => JFloat, Long => JLong, Short => JShort}

trait AttributeFormat[T] {
	def encode(data: T): String
	def decode(data: String): T
}

//noinspection ConvertibleToMethodValue
object AttributeFormat {
	class Generic[T](encoder: T => String, decoder: String => T) extends AttributeFormat[T] {
		final def encode(data: T): String = encoder(data)
		final def decode(data: String): T = decoder(data)
	}

	implicit object StringFormat extends Generic[String](identity, identity)
	implicit object CharFormat extends Generic[Char](Character.toString, _.charAt(0))
	implicit object ByteFormat extends Generic[Byte](JByte.toString, JByte.valueOf(_))
	implicit object ShortFormat extends Generic[Short](JShort.toString, JShort.valueOf(_))
	implicit object IntFormat extends Generic[Int](Integer.toString, Integer.valueOf(_))
	implicit object LongFormat extends Generic[Long](JLong.toString, JLong.valueOf(_))
	implicit object FloatFormat extends Generic[Float](JFloat.toString, JFloat.valueOf(_))
	implicit object DoubleFormat extends Generic[Double](JDouble.toString, JDouble.valueOf(_))

	implicit object BooleanFormat extends AttributeFormat[Boolean] {
		def encode(data: Boolean): String = if (data) "true" else "false"
		def decode(data: String): Boolean = data match {
			case "" | "0" | "false" | "no" => false
			case _ => true
		}
	}
}
