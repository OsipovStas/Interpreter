package ru.spbau.osipov.inter

import org.specs2.mutable.Specification
import ru.spbau.osipov.inter.interpreter._


/**
 * @author stasstels
 * @since 1/24/14.
 */
class ExpressionSpec extends Specification {

  "Expressions " should {
    "be interpret as 9894254525843536457346238934" in {
      val repr: String = "9894254525843536457346238934"
      eval(repr) mustEqual Right(IntNumber(BigInt(repr)))
    }

    "be interpret as -533451.2354545322346334534525335434544e342" in {
      val repr: String = "-533451.2354545322346334534525335434544e342"
      eval(repr) mustEqual Right(RealNumber(BigDecimal(repr)))
    }

    "be interpret as false" in {
      val repr: String = "F"
      eval(repr) mustEqual Right(False)
    }

    "be interpret as chars" in {
      val repr: String =
        """
          |"asdasg dfgsыв ывапыва\n fsdf\t ваывао ыва"
        """.stripMargin
      eval(repr) mustEqual Right(Chars(repr.trim))
    }

  }

}
