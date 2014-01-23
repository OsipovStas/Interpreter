package ru.spbau.osipov.inter.parser

/**
 * @author stasstels
 * @since 1/20/14.
 */
import scala.util.parsing.combinator._
import scala.language.implicitConversions

import ru.spbau.osipov.inter.interpreter._
import ru.spbau.osipov.inter.interpreter.IntNumber
import ru.spbau.osipov.inter.interpreter.RealNumber
import ru.spbau.osipov.inter.Executable
import ru.spbau.osipov.inter.Interpreter.Errors



trait Parsing {
  type R
  
  val Parser: LanguageParser[R]

  trait LanguageParser[R] {
    def parseProgram(program: String): Either[Errors, Executable[R]]
  }


  class ExpressionParser extends BaseParser with LanguageParser[Value] {

    def parseProgram(program: String) = parseAll(expr, program) match {
      case Success(e, _) => Right(e)
      case a: NoSuccess => Left(Seq(a.msg))
    }
  }

}

abstract class BaseParser extends JavaTokenParsers {

  def expr: Parser[Expression] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case first ~ terms => binOperationReduce(first, terms.map(t => (t._1, t._2)))
  }

  def term: Parser[Expression] = factor ~ rep("*" ~ factor | "/" ~ factor | "%" ~ factor) ^^ {
    case first ~ factors => binOperationReduce(first, factors.map(f => (f._1, f._2)))
  }

  def factor: Parser[Expression] = atom | "(" ~> expr <~ ")"

  def atom: Parser[Expression] = liter | call



  def liter: Parser[Expression] =
    floatingPointNumber ^^ { repr => ValExpression(str2Number(repr))} |
      stringLiteral ^^ { repr => ValExpression(Chars(repr))} |
      "T" ^^ {_ => ValExpression(True)} |
      "F" ^^ {_ => ValExpression(False)}

  def call = ident ~ opt(locals) ^^ {
    case i ~ None => VarExpression(i)
    case i ~ Some(l) => CallExpression(i, l)
  }

  def locals: Parser[List[Expression]] = "(" ~> repsep(expr, ",") <~ ")"

  def binOperationReducer(next: (String, Expression), current: Option[(String, Expression)]) = current map {
    case (curOp, right) => (next._1, BinaryExpression(curOp, next._2, right))
  } orElse Some(next)

  def binOperationReduce(mostLeft: Expression, other: List[(String, Expression)]): Expression =
    other.foldRight[Option[(String, Expression)]](None)(binOperationReducer).map(r => BinaryExpression(r._1, mostLeft, r._2)).getOrElse(mostLeft)



  def str2Number(repr: String): Number = str2bigInt(repr).map(IntNumber).getOrElse(RealNumber(BigDecimal(repr)))


  def str2bigInt(repr: String): Option[BigInt] =  {
    try {
      Some(BigInt(repr))
    }
    catch {
      case ex: NumberFormatException => None
    }
  }
}

object ParserInstance extends BaseParser
