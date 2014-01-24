package ru.spbau.osipov.inter.interpreter

import ru.spbau.osipov.inter.Interpreter._
import ru.spbau.osipov.inter.errors.Errors._
import ru.spbau.osipov.inter.{Interpreter, Executable}

/**
 * @author stasstels
 * @since 1/19/14.
 */


sealed abstract class Expression extends Executable[Value] {

  final def execute(ctx: Ctx): Either[Errors, Value] = eval(ctx)


  final def join(values: Values, value: Val): Values = value match {
    case Right(v) => values.right.map(_ :+ v)
    case Left(e) if values.isRight => Left(e)
    case Left(e) => values.left.map(_ ++ e)
  }


  def eval(ctx: Ctx): Val
}

case class ValExpression(value: Value) extends Expression {
  def eval(ctx: Ctx): Val = Right(value)
}



case class VarExpression(name: Var) extends Expression {
  def eval(ctx: Ctx): Val = ctx.get(name).map(Right(_)).getOrElse(Left(noDefFound(name)))
}



case class CallExpression(function: Var, args: Seq[Expression]) extends Expression {

  private val emptyArgs: Values = Right(Seq())

  def createLocalCtx(ctx: Ctx, bindings: Seq[Var]): Either[Seq[String], Ctx] = {
    evalArgs(ctx).right flatMap {
      case values if bindings.size == values.size => Right((bindings zip values).toMap)
      case values => Left(wrongParameters(bindings.toString(), values.toString()))
    }
  }

  def evalArgs(ctx: Ctx): Values = args.map(_ eval ctx).foldLeft(emptyArgs)(join)


  def eval(ctx: Ctx): Val = ctx.get(function).toRight(noDefFound(function)).right flatMap {
    case Function(bindings, body, scope) => createLocalCtx(ctx, bindings).right flatMap {
      case locals => body.exec(locals ++ scope).right flatMap {
        case t => t.get(Return).toRight(Seq("fsf"))
      }
    }
    case err => Left(functionExpected(err.toString))
  }
}

case class BinaryExpression(op: BinaryOp, left: Expression, right: Expression) extends Expression {
  def eval(ctx: Ctx): Val = left.eval(ctx).right flatMap {
    case l => right.eval(ctx).right flatMap {
      case r => op(l, r)
    }
  }
}

case class LazyBinExpression(op: LazyBinaryOp, left: Expression, right: Expression) extends Expression {
  def eval(ctx: Ctx): Val = left.eval(ctx).right flatMap {
    case l => op(l,  right.eval(ctx))
  }
}

case class UnaryExpression(op: UnaryOp, e: Expression) extends Expression {
  def eval(ctx: Ctx): Val = e.eval(ctx).right flatMap {
    case v => op(v)
  }
}