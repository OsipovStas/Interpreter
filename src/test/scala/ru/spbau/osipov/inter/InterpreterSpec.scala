package ru.spbau.osipov.inter
import org.specs2.mutable.Specification
import org.specs2.matcher.Matcher
import ru.spbau.osipov.inter.interpreter._
import ru.spbau.osipov.inter.interpreter.IntNumber

/**
 * @author stasstels
 * @since 1/24/14.
 */
class InterpreterSpec extends Specification {

  def haveCorrect(result: Value): Matcher[String] = be_==(result) ^^ {
    program: String => eval(program).right.get match {
      case ctx => eval.result(ctx).get
    }
  }

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
        """.stripMargin,
        """
          |def fact(n) {
          |   if (n < 3) {
          |      n
          |   } else {
          |     n * fact(n - 1)
          |   }
          |};
          |fact(9)
        """.stripMargin)
      val results = Seq(9, 362880).map(IntNumber(_))
      (programs zip results).map {
        case (program, result) => program must haveCorrect(result)
      }
    }

    "correct execute branches " in {
      val programs = Seq(
        """
          |x = 7;
          |y = 8;
          |if (x < y && y > 1 && not(x < 2)) {
          |   res = "Less"
          |};
          |res
        """.stripMargin,
        """
          |x = 7;
          |y = 8;
          |
          |if (x == y) {
          |   res = "equal"
          |} else {
          |   res = "Different"
          |};
          |res
        """.stripMargin,
        """
          |x = 7;
          |y = 8;
          |if (x <= 7) {
          |   res = "Lesser"
          |} else {
          |   res = "Greater"
          |};
          |res
        """.stripMargin,
        """
          |x = 7;
          |y = 8;
          |if (x >= 77) {
          |   res = "Greater"
          |} else {
          |   res = "Lesser"
          |};
          |res
        """.stripMargin
      )
      val results = Seq("\"Less\"", "\"Different\"", "\"Lesser\"", "\"Lesser\"").map(Chars)
      (programs zip results).map {
        case (program, result) => program must haveCorrect(result)
      }
    }


    "correct execute struct expressions " in {
      val programs = Seq(
        """
          |struct Person(name, age = 6, city = "New-York") {
          |   def getDoubleAge() {
          |       age * 2
          |   }
          |};
          |mrX = Person("John");
          |mrX.getDoubleAge()
        """.stripMargin,
        """
          |struct Person(name, salary, age = 6, city = "New-York") {
          |   def getDoubleAge() {
          |       age * 2
          |   }
          |};
          |mrX = Person("John", 45);
          |mrX.salary = 10;
          |mrX.salary
        """.stripMargin)
      val results = Seq(12, 10).map(IntNumber(_))
      (programs zip results).map {
        case (program, result) => program must haveCorrect(result)
      }
    }


    "correct execute loops " in {
      val programs = Seq(
        """
          |x = 3;
          |while (x < 19) {
          | x = x + 1
          |};
          |
          |x == 19
        """.stripMargin,
        """
          |y = 2;
          |x = 5;
          |while (x > 0 && (x + 1) > y) {
          | x = x - 1;
          | y = y + 1
          |};
          |
          |(x + 1) > y
        """.stripMargin)
      val results = Seq(True, False)
      (programs zip results) must beCorrectInterpret
    }

    "correct execute recursive functions " in {
      val programs = Seq(
        """
          |def fib(x) {
          |   if (x <= 1) {
          |     x
          |   } else {
          |     r = fib(x - 1);
          |     r + fib(x - 2)
          |   }
          |};
          |fib(15)
        """.stripMargin,
        """
          |def ack(m, n) {
          |   if (m == 0) {
          |     n + 1
          |   } else if (n == 0) {
          |     ack(m - 1, 1)
          |   } else {
          |     ack(m - 1, ack(m, n - 1))
          |   }
          |};
          |ack(3, 6)
        """.stripMargin)
      val results = Seq(610, 509).map(IntNumber(_))
      (programs zip results).map {
        case (program, result) => program must haveCorrect(result)
      }
    }

  }
}
