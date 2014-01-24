package ru.spbau.osipov.inter

import ru.spbau.osipov.inter.interpreter.{Node, Value}
import ru.spbau.osipov.inter.Interpreter.Ctx
import ru.spbau.osipov.inter.parser.Parsing
import ru.spbau.osipov.inter.errors.Errors.Errors

/**
 * @author stasstels
 * @since 1/23/14.
 */

trait Executable[R] {
  def execute(ctx: Ctx): Either[Errors, R]
}

trait Executing { this: Parsing =>

  val Executor: Executor[R]

  trait Executor[T] {
    def execute(e: Executable[T]): Either[Errors, T]
  }

  object FreeExecutor extends Executor[Value] {
    def execute(e: Executable[Value]): Either[Errors, Value] = e.execute(Map())
  }

  case class CtxExecutor[T](ctx: Ctx) extends Executor[T] {
    def execute(e: Executable[T]): Either[Errors, T] = e.execute(ctx)
  } 
  
  
}


object Interpreter {
  type Val = Either[Errors, Value]
  type Values = Either[Errors, Seq[Value]]
  type Var = String
  type Ctx = Map[Var, Value]
  type Env = Either[Errors, Ctx]

  val Return: Var = "$RETURN"
}

object eval extends Parsing with Executing {
  type R = Node
  val Executor: eval.Executor[eval.R] = CtxExecutor(Map())
  val Parser: eval.LanguageParser[eval.R] = MainParser

  def apply(program: String): Either[Errors, eval.R] = Parser.parseProgram(program).right.flatMap(Executor.execute)
}



object expr extends Parsing with Executing {
  type R = Value
  val Parser: expr.LanguageParser[expr.R] = ExpressionParser
  val Executor: expr.Executor[expr.R] = FreeExecutor

  def apply(program: String): Either[Errors, expr.R] = Parser.parseProgram(program).right.flatMap(Executor.execute)
}

object Repl extends App {

//  def loop: Stream[String] = eval(readLine()) #:: loop
//
//  def apply() = loop.foreach(println(_))
//
//  Repl()
}