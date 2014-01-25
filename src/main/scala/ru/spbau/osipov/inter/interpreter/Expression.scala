package ru.spbau.osipov.inter.interpreter

import ru.spbau.osipov.inter.Defines._
import ru.spbau.osipov.inter.errors.Errors._
import ru.spbau.osipov.inter.Executable

/**
 * @author stasstels
 * @since 1/19/14.
 */


sealed abstract class Expression extends Executable[Value] {

  final def execute(ctx: Ctx): Either[Errors, Value] = eval(ctx)


  def eval(ctx: Ctx): Val
}

case class ValExpression(value: Value) extends Expression {
  def eval(ctx: Ctx): Val = Right(value)
}



case class VarExpression(name: Var) extends Expression {
  def eval(ctx: Ctx): Val = ctx.get(name).map(Right(_)).getOrElse(Left(noDefFound(name)))
}

case class ConsExpression(tag: Var) extends Expression {
  def eval(ctx: Ctx): Val = Right(Structure(tag + "$", ctx - tag))
}

case class FieldExpression(name: Var, field: Var) extends Expression {
  def eval(ctx: Ctx): Val = ctx.get(name).map {
    case Structure(_, fields) => fields.get(field).map(Right(_)).getOrElse(Left(noDefFound(field)))
    case s => Left(structureExpected(s.toString))
  } getOrElse Left(noDefFound(name))
}


abstract class Call(args: Seq[Expression]) extends Expression {
  private val emptyArgs: Values = Right(Seq())
  protected val void: Value = Single

  private def join(values: Values, value: Val): Values = value match {
    case Right(v) => values.right.map(_ :+ v)
    case Left(e) if values.isRight => Left(e)
    case Left(e) => values.left.map(_ ++ e)
  }



  def createLocalCtx(ctx: Ctx, bindings: Seq[Var]): Either[Seq[String], Ctx] = {
    evalArgs(ctx).right flatMap {
      case values => Right((bindings zip values).toMap)
    }
  }

  def evalArgs(ctx: Ctx): Values = args.map(_ eval ctx).foldLeft(emptyArgs)(join)
}


case class MethodCall(name: Var, method: Var, args: Seq[Expression]) extends Call(args) {
  
  def eval(ctx: Ctx): Val = ctx.get(name).map {
    case obj @ Structure(tag, fields) => ctx.get(tag + method).map {
      case m @ Function(_, _, _) => evalMethod(obj, m, ctx)
      case v => Left(functionExpected(v.toString))
    } getOrElse Left(noDefFound(method))
    case v => Left(structureExpected(v.toString))
  } getOrElse Left(noDefFound(name))
  
  def evalMethod(obj: Structure, method: Function, ctx: Ctx): Val = createLocalCtx(ctx, method.bindings).right flatMap {
    case locals => method.body.exec(method.scope ++ obj.fields ++ locals + (name + obj.tag -> method)).right map {
      case r => r.getOrElse(Return, void)
    }
  }
}



case class FunctionCall(function: Var, args: Seq[Expression]) extends Call(args) {

  def eval(ctx: Ctx): Val = ctx.get(function).toRight(noDefFound(function)).right flatMap {
    case f @ Function(bindings, body, scope) => createLocalCtx(ctx, bindings).right flatMap {
      case locals => body.exec(scope ++ locals + (function -> f)).right map {
        case r => r.getOrElse(Return, void)
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