package ru.spbau.osipov.inter.interpreter

import scala.annotation.tailrec

import ru.spbau.osipov.inter.Interpreter._
import ru.spbau.osipov.inter.{Interpreter, Executable}
import ru.spbau.osipov.inter.errors.Errors.Errors

/**
 * @author stasstels
 * @since 1/19/14.
 */
sealed abstract class Node extends Executable[Ctx] {


  final def execute(ctx: Ctx): Either[Errors, Ctx] = exec(ctx)

  def exec(ctx: Ctx): Env

  def expectedLogicValue(actual: String) = Seq(
    s"""
      |Expected logic value
      |Actual: $actual
    """.stripMargin
  )

}

case object SkipNode extends Node {
  def exec(ctx: Ctx): Env = Right(ctx)
}

case class ExpressionNode(expr: Expression) extends Node {
  def exec(ctx: Ctx): Env = expr.eval(ctx).right map {
    case v => ctx + (Return -> v)
  }
}

case class AssignVar(name: Var, value: Expression) extends Node {
  def exec(ctx: Ctx): Env = value.eval(ctx).right map {
    case v => ctx + (name -> v)
  }
}

case class PrintNode(value: Expression) extends Node {
  def exec(ctx: Ctx): Env = value.eval(ctx).right map {
    case v =>
      println(v)
      ctx
  }
}

case class ClipNode(first: Node, second: Node) extends Node {
  def exec(ctx: Ctx): Env = first.exec(ctx).right flatMap {
    case newCtx => second.exec(newCtx)
  }
}

case class BranchNode(cond: Expression, body: Node, alter: Node) extends Node {
  def exec(ctx: Ctx): Env = cond.eval(ctx).right flatMap {
    case True => body.exec(ctx)
    case False => alter.exec(ctx)
    case v => Left(expectedLogicValue(v.toString))
  }
}

case class LoopNode(cond: Expression, body: Node) extends Node {


  @tailrec
  final def exec(ctx: Ctx): Env = {
    var continue = false
    val nCtx: Either[Seq[String], Ctx] = cond.eval(ctx).right flatMap {
      case True =>
        continue = true
        body.exec(ctx)
      case False => Right(ctx)
      case v => Left(expectedLogicValue(v.toString))
    }
    nCtx match {
      case Left(e) => nCtx
      case Right(_) if !continue => nCtx
      case Right(ct) => exec(ct)
    }
  }
}

case class FunctionNode(name: Var, bindings: Seq[(Var, Option[Expression])], body: Node) extends Node {

  def argNames = bindings.map(_._1)

  def evalBindings(ctx: Ctx): Seq[(Var, Val)] = bindings.filterNot(_._2.isEmpty).map(t => (t._1, t._2.get.eval(ctx)))

  def defaultBindings(vals: Seq[(Var, Val)] ) = vals.filter(_._2.isLeft).map(_._2.left.get) match {
    case s if s.isEmpty => Right(vals.map(t => (t._1, t._2.right.get)).toMap)
    case s => Left(s.foldLeft[Errors](Seq())(_ ++ _))
  }

  def exec(ctx: Ctx): Env = defaultBindings(evalBindings(ctx)).right.map {
    case scope => ctx + (name -> Function(argNames, body, ctx ++ scope))
  }
}