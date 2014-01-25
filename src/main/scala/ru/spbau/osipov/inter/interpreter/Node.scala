package ru.spbau.osipov.inter.interpreter


import ru.spbau.osipov.inter.Defines._
import ru.spbau.osipov.inter.{Defines, Executable}
import ru.spbau.osipov.inter.errors.Errors._
import scala.util.control.TailCalls._

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

case class AssignField(obj: Var, field: Var, value: Expression) extends Node {
  def exec(ctx: Ctx): Env = ctx.get(obj).map {
    case Structure(t, fields) => value.eval(ctx).right map {
      case v => ctx + (obj -> Structure(t, fields + (field -> v)))
    }
    case v => Left(structureExpected(v.toString))
  } getOrElse Left(noDefFound(obj))
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

  def execRec(ctx: Ctx): TailRec[Env] = cond.eval(ctx).fold ({
    case err => done(Left(err))
  }, {
    case False => done(Right(ctx))
    case True => tailcall(loopStep(ctx))
    case v => done(Left(expectedLogicValue(v.toString)))
  })

  def loopStep(ctx: Ctx): TailRec[Env] = body.exec(ctx).fold ({
    case err => done(Left(err))
  }, {
    case nCtx => tailcall(execRec(nCtx))
  })

  def exec(ctx: Defines.Ctx): Env = execRec(ctx).result
}

abstract class DefineNode(bindings: Seq[(Var, Option[Expression])]) extends Node {
  def argNames = bindings.map(_._1)

  def evalBindings(ctx: Ctx): Seq[(Var, Val)] = bindings.filterNot(_._2.isEmpty).map(t => (t._1, t._2.get.eval(ctx)))

  def defaultBindings(vals: Seq[(Var, Val)]) = vals.filter(_._2.isLeft).map(_._2.left.get) match {
    case s if s.isEmpty => Right(vals.map(t => (t._1, t._2.right.get)).toMap)
    case s => Left(s.foldLeft[Errors](Seq())(_ ++ _))
  }
}



case class StructureNode(name: Var, fields: Seq[(Var, Option[Expression])]) extends DefineNode(fields) {

  def body = ExpressionNode(ConsExpression(name))

  def exec(ctx: Ctx): Env = defaultBindings(evalBindings(ctx)).right.map {
    case scope => ctx + (name -> Function(argNames, body, ctx ++ scope))
  }
}





case class FunctionNode(name: Var, bindings: Seq[(Var, Option[Expression])], body: Node) extends DefineNode(bindings) {

  def exec(ctx: Ctx): Env = defaultBindings(evalBindings(ctx)).right.map {
    case scope => ctx + (name -> Function(argNames, body, ctx ++ scope))
  }
}