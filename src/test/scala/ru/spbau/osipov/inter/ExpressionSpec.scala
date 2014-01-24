package ru.spbau.osipov.inter

import org.specs2.mutable.Specification
import ru.spbau.osipov.inter.interpreter._
import org.specs2.matcher.{MatchResult, Expectable, Matcher}


/**
 * @author stasstels
 * @since 1/24/14.
 */
class ExpressionSpec extends Specification {

  def beCorrectInterpret: Matcher[Seq[(String, Value)]] = forall(be_==(true) ^^ {
    t: (String, Value) => eval(t._1).right.get == t._2
  })


  "Expressions " should {
    "correct interpret int literals" in {
      val repr: String = "9894254525843536457346238934"
      eval(repr) mustEqual Right(IntNumber(BigInt(repr)))
    }

    "correct interpret float literals" in {
      val repr: String = "-533451.2354545322346334534525335434544e342"
      eval(repr) mustEqual Right(RealNumber(BigDecimal(repr)))
    }

    "correct interpret logic values" in {
      val repr: String = "F"
      eval(repr) mustEqual Right(False)
    }

    "not overflow" in {
      val repr: String = "3.3424245 + (38324234252352352323 + 2423423423423234131)"
      eval(repr) mustEqual Right(RealNumber(BigDecimal("40747657675775586457.3424245")))
    }

    "correct interpret string literals " in {
      val programs = Seq(
        """
          |"зылвовпаовпдjdlfns"
        """.stripMargin,
        """
          |"fkjsakjfdkasjfla"
        """.stripMargin,
        """
          | "!##№№24\\\nsdfsd;%:;"
        """.stripMargin)
      val results = programs.map(_.trim).map(Chars)
      (programs zip results) must beCorrectInterpret
    }

    "correct eval binary operators" in {
      val repr: String = "3424234 % 425 * 2 / (-234 - 7)"
      val repr2: String = "4 + 342423.4 % 42.5 * (1.0 - 5)"
      val value: BigDecimal = 4 + 342423.4 % 42.5 * (1.0 - 5)
      eval(repr) mustEqual Right(IntNumber(3424234 % 425 * 2 / (-234 - 7)))
      (eval(repr2).right.get match {case RealNumber(v) if (v - value).abs < 0.001  => true}) mustEqual true
    }


    "correct eval ordered operators " in {
      val programs = Seq("4 < 5", "4 < 3", "4 > 4", "(3 + 1) >= -3.5", "T < F")
      val results = Seq(4 < 5, 4 < 3, 4 > 4, (3 + 1) >= -3.5, true < false).map(Logic(_))
      (programs zip results) must beCorrectInterpret
    }


  }

}
