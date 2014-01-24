package ru.spbau.osipov.inter
import org.specs2.mutable.Specification
import org.specs2.matcher.Matcher
import ru.spbau.osipov.inter.interpreter.{IntNumber, Value}

/**
 * @author stasstels
 * @since 1/24/14.
 */
class InterpreterSpec extends Specification {
  def beCorrectInterpret: Matcher[Seq[(String, Value)]] = forall(beTrue ^^ {
    t: (String, Value) => eval(t._1).right.get match {
      case ctx => eval.result(ctx).get == t._2
    }
  })


  "Interpreter " should {
    "correct execute expressions " in {
      val programs = Seq(
        """
          |432
        """.stripMargin,
        """
          |42342.23424;
          |4324 + 4242 * 32 % (92 - 4);
          |-53
        """.stripMargin)
      val results = Seq(432, -53).map(IntNumber(_))
      (programs zip results) must beCorrectInterpret
    }

    "correct execute assigns " in {
      val programs = Seq(
        """
          |x = 5 + 6;
          |y = 10 - 2 * (x % 21);
          |y
        """.stripMargin)
      val results = Seq(-12).map(IntNumber(_))
      (programs zip results) must beCorrectInterpret
    }

    "correct execute functions " in {
      val programs = Seq(
        """
          |def foo(a, b, c = 0) {
          |   a + b + c
          |};
          |
          |foo(3, 5 + 1)
        """.stripMargin)
      val results = Seq(9).map(IntNumber(_))
      (programs zip results) must beCorrectInterpret
    }


  }
}
