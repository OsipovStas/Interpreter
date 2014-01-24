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

    "not overflow" in {
      val repr: String = "3.3424245 + (38324234252352352323 + 2423423423423234131)"
      eval(repr) mustEqual Right(RealNumber(BigDecimal("40747657675775586457.3424245")))
    }

    "be interpret as chars" in {
      val repr: String =
        """
          |"asdasg dfgsыв ывапыва\n fsdf\t ваывао ыва"
        """.stripMargin
      eval(repr) mustEqual Right(Chars(repr.trim))
    }

    "correct eval binary operators" in {
      val repr: String = "3424234 % 425 * 2 / (-234 - 7)"
      val repr2: String = "4 + 342423.4 % 42.5 * (1.0 - 5)"
      val value: BigDecimal = 4 + 342423.4 % 42.5 * (1.0 - 5)
      eval(repr) mustEqual Right(IntNumber(3424234 % 425 * 2 / (-234 - 7)))
      (eval(repr2).right.get match {case RealNumber(v) if (v - value).abs < 0.001  => true}) mustEqual true
    }


  }

}
