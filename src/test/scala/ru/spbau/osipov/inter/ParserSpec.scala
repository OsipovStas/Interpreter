package ru.spbau.osipov.inter

import org.specs2.mutable.Specification
import ru.spbau.osipov.inter.interpreter._
import ru.spbau.osipov.inter.interpreter.IntNumber
import ru.spbau.osipov.inter.interpreter.ValExpression
import scala.Some
import ru.spbau.osipov.inter.interpreter.RealNumber
import ru.spbau.osipov.inter.parser.ParserInstance


/**
 * @author stasstels
 * @since 1/22/14.
 */
class ParserSpec extends Specification {

  val PI = ParserInstance

  "ParserResult " can {
    "be integer literal " in {
      val repr: String = "9894254525843536457346238934"
      val parseResult: PI.ParseResult[Expression] = PI.parseAll(PI.expr, repr)
      parseResult.map {
        case ValExpression(IntNumber(value))  => Some(value)
        case _ => None
      } getOrElse None mustEqual Some(BigInt(repr))
    }

    "be float literal " in {
      val repr: String = "11234509745258435364.86421149557346238934"
      val parseResult: PI.ParseResult[Expression] = PI.parseAll(PI.expr, repr)
      parseResult.map {
        case ValExpression(RealNumber(value))  => Some(value)
        case _ => None
      } getOrElse None mustEqual Some(BigDecimal(repr))
    }

    "be boolean literal " in {
      val repr: String = "T"
      val parseResult: PI.ParseResult[Expression] = PI.parseAll(PI.expr, repr)
      parseResult.map {
        case ValExpression(True)  => Some(true)
        case _ => None
      } getOrElse None mustEqual Some(true)
    }

    "be string literal " in {
      val repr: String =
        """
          |"String Literal!!частпп2642:%?:;:%№:;№#@#$5 \n fsdf"
        """.stripMargin
      val parseResult: PI.ParseResult[Expression] = PI.parseAll(PI.expr, repr)
      parseResult.map {
        case ValExpression(Chars(value))  => Some(value)
        case _ => None
      } getOrElse None mustEqual Some(repr.trim)
    }

    "be function call " in {
      val repr: String =
        """
          |f(4+5, 5 % (func(4) - x))
        """.stripMargin
      val parseResult: PI.ParseResult[Expression] = PI.parseAll(PI.expr, repr)
      parseResult.map {
        case CallExpression(f, _)  => Some(f)
        case _ => None
      } getOrElse None mustEqual Some("f")
    }

    "be variable " in {
      val repr: String =
        """
          |someStupidIdentificat0rСРусскимиБуквамиAndNumb3rs
        """.stripMargin
      val parseResult: PI.ParseResult[Expression] = PI.parseAll(PI.expr, repr)
      parseResult.map {
        case VarExpression(name)  => Some(name)
        case _ => None
      } getOrElse None mustEqual Some(repr.trim)
    }



  }
}
