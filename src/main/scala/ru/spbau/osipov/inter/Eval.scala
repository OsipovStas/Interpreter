package ru.spbau.osipov.inter

import ru.spbau.osipov.inter.interpreter.Value
import ru.spbau.osipov.inter.Interpreter.{Errors, Ctx}
import ru.spbau.osipov.inter.parser.Parsing

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

  class FreeExecutor extends Executor[Value] {
    def execute(e: Executable[Value]): Either[Errors, Value] = e.execute(Map())
  }

}


object Interpreter {
  type Errors = Seq[String]
  type Val = Either[Errors, Value]
  type Values = Either[Errors, Seq[Value]]
  type Var = String
  type Ctx = Map[Var, Value]
  type Env = Either[Errors, Ctx]


  val Return: Var = "$RETURN"

}


object eval extends Parsing with Executing {
  type R = Value
  val Parser: eval.LanguageParser[eval.R] = new ExpressionParser
  val Executor: eval.Executor[eval.R] = new FreeExecutor

  def apply(program: String): Either[Errors, eval.R] = Parser.parseProgram(program).right.flatMap(Executor.execute)
}

object Repl extends App {

//  def loop: Stream[String] = eval(readLine()) #:: loop
//
//  def apply() = loop.foreach(println(_))
//
//  Repl()
}