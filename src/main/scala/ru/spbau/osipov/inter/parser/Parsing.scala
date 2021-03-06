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
import ru.spbau.osipov.inter.errors.Errors.Errors
import ru.spbau.osipov.inter.Defines.Ctx


trait Parsing {
  type R
  
  val Parser: LanguageParser[R]

  trait LanguageParser[R] {
    def parseProgram(program: String): Either[Errors, Executable[R]]
  }


  object ExpressionParser extends BaseParser with LanguageParser[Value] {

    def parseProgram(program: String) = parseAll(expr, program) match {
      case Success(e, _) => Right(e)
      case a: NoSuccess => Left(Seq(a.msg))
    }
  }

  object MainParser extends BaseParser with LanguageParser[Ctx] {
    def parseProgram(src: String): Either[Errors, Executable[Ctx]] = parseAll(code, src) match {
      case Success(e, _) => Right(e)
      case a: NoSuccess => Left(Seq(a.msg))
    }
  }

}

abstract class BaseParser extends JavaTokenParsers {

  def code: Parser[Node] = statement ~ rep(";" ~> statement) ^^ {
    case first ~ other => other.foldLeft(first)(ClipNode)
  }

  def statement: Parser[Node] = struct | loop | branch | define | assign | ex


  def struct: Parser[Node] = constructor ~ opt(methods) ^^ {
    case n ~ p ~ Some(m) => m.map {
      case i ~ s ~ b => FunctionNode(n + "$" + i, s, b)
    }.foldLeft[Node](StructureNode(n, p))(ClipNode)
    case n ~ p ~ None => StructureNode(n, p)
  }

  def constructor = "struct" ~> ident ~ sign

  def methods = "{" ~> rep1(method) <~ "}"

  def method = "def" ~> ident ~ sign ~ block

  def loop: Parser[Node] = "while" ~> ("(" ~> expr <~ ")") ~ block ^^ {
    case cond ~ body => LoopNode(cond, body)
  }
  
  def branch: Parser[Node] = "if" ~> ("(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ body ~ Some(alter) => BranchNode(cond, body, alter)
    case cond ~ body ~ None => BranchNode(cond, body, SkipNode)
  }
  
  
  def define: Parser[Node] = "def" ~> ident ~ sign ~ block ^^ {
    case i ~ s ~ b => FunctionNode(i, s, b)
  }

  def sign = "(" ~> repsep(param, ",") <~ ")" ^^ {
    case params => params.map {
      case n ~ e => (n, e)
    }
  }

  def param = ident ~ opt("=" ~> expr)
  
  def block = "{" ~> code <~ "}" | statement

  def assign: Parser[Node] = (ident ~ opt("." ~> ident) <~ "=") ~ expr ^^ {
    case i1 ~ None ~ e => AssignVar(i1, e)
    case i1 ~ Some(i2) ~ e => AssignField(i1, i2, e)
  }

  def ex: Parser[Node] = expr ^^ (e => ExpressionNode(e))


  def expr: Parser[Expression] = disj ~ rep("||" ~ disj) ^^ {
    case first ~ other => buildLazyBinOpTree(first, other.map(t => (t._1, t._2)))
  }

  def disj: Parser[Expression] = conj ~ rep("&&" ~ conj) ^^ {
    case first ~ other => buildLazyBinOpTree(first, other.map(t => (t._1, t._2)))
  }

  def conj: Parser[Expression] = eq ~ rep("==" ~ eq | "!=" ~ eq) ^^ {
    case first ~ other => buildBinOpTree(first, other.map(t => (t._1, t._2)))
  }

  def eq: Parser[Expression] = lge ~ rep("<=" ~ lge | ">=" ~ lge) ^^ {
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

  def factor: Parser[Expression] = opt("not" | "-") ~ unary ^^ {
    case Some(u) ~ e => UnaryExpression(u, e)
    case None ~ e => e
  }

  def unary: Parser[Expression] = atom | "(" ~> expr <~ ")"

  def atom: Parser[Expression] = liter | call



  def liter: Parser[Expression] =
    floatingPointNumber ^^ { repr => ValExpression(str2Number(repr))} |
      stringLiteral ^^ { repr => ValExpression(Chars(repr))} |
      "T" ^^ {_ => ValExpression(True)} |
      "F" ^^ {_ => ValExpression(False)}

  def call = ident ~ opt("." ~> ident) ~ opt(locals) ^^ {
    case i ~ None ~ None => VarExpression(i)
    case i ~  None ~ Some(l) => FunctionCall(i, l)
    case i1 ~ Some(i2) ~ None => FieldExpression(i1, i2)
    case i1 ~ Some(i2) ~ Some(l) => MethodCall(i1, i2, l)
  }

  def locals: Parser[List[Expression]] = "(" ~> repsep(expr, ",") <~ ")"

  def reducer(left: Expression, next: (String, Expression)) = BinaryExpression(next._1, left, next._2)

  def lazyReducer(left: Expression, next: (String, Expression)) = LazyBinExpression(next._1, left, next._2)


  def buildBinOpTree(first: Expression, other: Seq[(String, Expression)]) = other.foldLeft(first)(reducer)

  def buildLazyBinOpTree(first: Expression, other: Seq[(String, Expression)]) = other.foldLeft(first)(lazyReducer)


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
