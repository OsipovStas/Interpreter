package ru.spbau.osipov.inter

import ru.spbau.osipov.inter.interpreter.{Single, Value}
import ru.spbau.osipov.inter.Defines.Ctx
import ru.spbau.osipov.inter.parser.Parsing
import ru.spbau.osipov.inter.errors.Errors.Errors
import scala.annotation.tailrec

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

  case class CtxExecutor(start: Ctx) extends Executor[Ctx] {
    def execute(e: Executable[Ctx]): Either[Errors, Ctx] = e.execute(start)


  }


  
}

object Defines {
  type Val = Either[Errors, Value]
  type Values = Either[Errors, Seq[Value]]
  type Var = String
  type Ctx = Map[Var, Value]
  type Env = Either[Errors, Ctx]

  val Return: Var = "$RETURN"
}

object eval extends Parsing with Executing {
  type R = Ctx
  val Executor: eval.Executor[eval.R] = CtxExecutor(Map())
  val Parser: eval.LanguageParser[eval.R] = MainParser

  def apply(program: String): Either[Errors, eval.R] = Parser.parseProgram(program).right.flatMap(Executor.execute)

  def result(ctx: eval.R) = ctx.get(Defines.Return)




  @tailrec
  final def loop(ctx: Ctx): Ctx = {
    readLine("-> ") match {
      case "exit" => ctx
      case program => Parser.parseProgram(program) match {
        case Right(e) => loop(e.execute(ctx).fold({
          case err =>
            println("Errors occurred: ")
            err.foreach(println(_))
            ctx
        }, {
          case nCtx =>
            printf("Result: %s\n", nCtx.getOrElse(Defines.Return, Single))
            nCtx
        }))
        case Left(err) =>
          println("Parsing errors occurred: ")
          err.foreach(println(_))
          loop(ctx)
      }
    }
  }


  def start(cmds: Seq[String]) = loop(Map())

}



object expr extends Parsing with Executing {
  type R = Value
  val Parser: expr.LanguageParser[expr.R] = ExpressionParser
  val Executor: expr.Executor[expr.R] = FreeExecutor

  def apply(program: String): Either[Errors, expr.R] = Parser.parseProgram(program).right.flatMap(Executor.execute)

}

object Repl extends App {

  def apply() = eval.loop(Map())

  Repl()
}