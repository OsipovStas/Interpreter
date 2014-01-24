package ru.spbau.osipov.inter.interpreter

import scala.language.implicitConversions
import ru.spbau.osipov.inter.Interpreter._
/**
 * @author stasstels
 * @since 1/19/14.
 */
sealed abstract class BinaryOp {
  def apply(v1: Value, v2: Value): Val

  def binaryException(what: String) = Seq(
    s"""
      |Exception in binary expression:
      |Msg: $what
    """.stripMargin
  )
}

case object !+! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(IntNumber(i1.value + i2.value))
      case (n1: Number, n2: Number) => Right(RealNumber(n1.getNumber + n2.getNumber))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}

case object !-! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(IntNumber(i1.value - i2.value))
      case (n1: Number, n2: Number) => Right(RealNumber(n1.getNumber - n2.getNumber))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}

case object !*! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(IntNumber(i1.value * i2.value))
      case (n1: Number, n2: Number) => Right(RealNumber(n1.getNumber * n2.getNumber))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}

case object !/! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(IntNumber(i1.value / i2.value))
      case (n1: Number, n2: Number) => Right(RealNumber(n1.getNumber / n2.getNumber))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}


case object !%! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(IntNumber(i1.value % i2.value))
      case (n1: Number, n2: Number) => Right(RealNumber(n1.getNumber % n2.getNumber))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}


object BinaryOp {
  implicit def str2binOp(repr: String): BinaryOp = repr match {
    case "+" => !+!
    case "-" => !-!
    case "*" => !*!
    case "/" => !/!
    case "%" => !%!
//    case "<" => !<!
//    case ">" => !>!
//    case "<=" => !<=!
//    case ">=" => !>=!
//    case "==" => !==!
//    case "!=" => !!=!
//    case "&&" => !&&!
//    case "||" => !||!
    case _ => !+!
  }
}