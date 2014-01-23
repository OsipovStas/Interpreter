package ru.spbau.osipov.inter.interpreter

import scala.annotation.tailrec

import ru.spbau.osipov.inter.Interpreter._

/**
 * @author stasstels
 * @since 1/19/14.
 */
sealed abstract class Node {

  def execute(ctx: Ctx): Env

  def expectedLogicValue(actual: String) = Seq(
    s"""
      |Expected logic value
      |Actual: $actual
    """.stripMargin
  )

}

case object SkipNode extends Node {
  def execute(ctx: Ctx): Env = Right(ctx)
}

case class ExpressionNode(expr: Expression) extends Node {
  def execute(ctx: Ctx): Env = expr.eval(ctx).right map {
    case v => ctx + (Return -> v)
  }
}

case class AssignVar(name: Var, value: Expression) extends Node {
  def execute(ctx: Ctx): Env = value.eval(ctx).right map {
    case v => ctx + (name -> v)
  }
}

case class PrintNode(value: Expression) extends Node {
  def execute(ctx: Ctx): Env = value.eval(ctx).right map {
    case v =>
      println(v)
      ctx
  }
}

case class ClipNode(first: Node, second: Node) extends Node {
  def execute(ctx: Ctx): Env = first.execute(ctx).right flatMap {
    case newCtx => second.execute(newCtx)
  }
}

case class BranchNode(cond: Expression, body: Node, alter: Node) extends Node {
  def execute(ctx: Ctx): Env = cond.eval(ctx).right flatMap {
    case True => body.execute(ctx)
    case False => alter.execute(ctx)
    case v => Left(expectedLogicValue(v.toString))
  }
}

case class LoopNode(cond: Expression, body: Node) extends Node {


  @tailrec
  final def execute(ctx: Ctx): Env = {
    var continue = false
    val nCtx: Either[Seq[String], Ctx] = cond.eval(ctx).right flatMap {
      case True =>
        continue = true
        body.execute(ctx)
      case False => Right(ctx)
      case v => Left(expectedLogicValue(v.toString))
    }
    nCtx match {
      case Left(e) => nCtx
      case Right(_) if !continue => nCtx
      case Right(ct) => execute(ct)
    }
  }
}

case class FunctionNode(name: Var, bindings: Seq[Var], body: Node) extends Node {
  def execute(ctx: Ctx): Env = Right(ctx + (name -> Function(bindings, body, ctx)))
}