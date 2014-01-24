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

  def expr: Parser[Expression] = lge ~ rep("<=" ~ lge | ">=" ~ lge) ^^ {
    case first ~ other => buildBinOpTree(first, other.map(t => (t._1, t._2)))
  }

  def lge: Parser[Expression] = lg ~ rep("<" ~ lg | ">" ~ lg) ^^ {
    case first ~ other => buildBinOpTree(first, other.map(t => (t._1, t._2)))
  }

  def lg: Parser[Expression] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case first ~ other => buildBinOpTree(first, other.map(t => (t._1, t._2)))
  }

  def term: Parser[Expression] = factor ~ rep("*" ~ factor | "/" ~ factor | "%" ~ factor) ^^ {
    case first ~ other => buildBinOpTree(first, other.map(t => (t._1, t._2)))
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

  def reducer(left: Expression, next: (String, Expression)) = BinaryExpression(next._1, left, next._2)

  def buildBinOpTree(first: Expression, other: Seq[(String, Expression)]) = other.foldLeft(first)(reducer)

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
