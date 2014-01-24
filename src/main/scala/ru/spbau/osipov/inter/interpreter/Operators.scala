package ru.spbau.osipov.inter.interpreter

import scala.language.implicitConversions
import ru.spbau.osipov.inter.Interpreter._
import ru.spbau.osipov.inter.Interpreter
import ru.spbau.osipov.inter.errors.Errors

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

case object !<! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(Logic(i1.value < i2.value))
      case (n1: Number, n2: Number) => Right(Logic(n1.getNumber < n2.getNumber))
      case (l1: Logic, l2: Logic) => Right(Logic(l1.value < l2.value))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}

case object !>! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(Logic(i1.value > i2.value))
      case (n1: Number, n2: Number) => Right(Logic(n1.getNumber > n2.getNumber))
      case (l1: Logic, l2: Logic) => Right(Logic(l1.value > l2.value))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}


case object !<=! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(Logic(i1.value <= i2.value))
      case (n1: Number, n2: Number) => Right(Logic(n1.getNumber <= n2.getNumber))
      case (l1: Logic, l2: Logic) => Right(Logic(l1.value <= l2.value))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}

case object !>=! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(Logic(i1.value >= i2.value))
      case (n1: Number, n2: Number) => Right(Logic(n1.getNumber >= n2.getNumber))
      case (l1: Logic, l2: Logic) => Right(Logic(l1.value >= l2.value))
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception  => Left(binaryException(ex.getMessage))
  }
}

case object !==! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(Logic(i1.value == i2.value))
      case (n1: Number, n2: Number) => Right(Logic(n1.getNumber == n2.getNumber))
      case (l1: Logic, l2: Logic) => Right(Logic(l1.value == l2.value))
      case (Chars(s1), Chars(s2)) => Right(Logic(s1 == s2))
      case (Single, Single) => Right(True)
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}

case object !!=! extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = try {
    (v1, v2) match {
      case (i1: IntNumber, i2: IntNumber) => Right(Logic(i1.value != i2.value))
      case (n1: Number, n2: Number) => Right(Logic(n1.getNumber != n2.getNumber))
      case (l1: Logic, l2: Logic) => Right(Logic(l1.value != l2.value))
      case (Chars(s1), Chars(s2)) => Right(Logic(s1 != s2))
      case (Single, Single) => Right(True)
      case _ => Left(binaryException("Bad types"))
    }
  }
  catch {
    case ex: Exception => Left(binaryException(ex.getMessage))
  }
}


case object BAD extends BinaryOp {
  def apply(v1: Value, v2: Value): Val = Left(Errors.binaryException("BAD operator"))
}


object BinaryOp {
  implicit def str2binOp(repr: String): BinaryOp = repr match {
    case "+" => !+!
    case "-" => !-!
    case "*" => !*!
    case "/" => !/!
    case "%" => !%!
    case "<" => !<!
    case ">" => !>!
    case "<=" => !<=!
    case ">=" => !>=!
    case "==" => !==!
    case "!=" => !!=!
    case _ => BAD
  }
}

sealed abstract class LazyBinaryOp {
  def apply(v1: Value, v2: => Val): Val
}

case object #&&# extends LazyBinaryOp {
  def apply(v1: Value, v2: => Val): Val = v1 match {
    case False => Right(False)
    case True if v2 == Right(True) => Right(True)
    case True if v2 == Right(False) => Right(False)
    case _ => Left(Errors.binaryException("Bad Types"))
  }
}

case object #||# extends LazyBinaryOp {
  def apply(v1: Value, v2: => Val): Val = v1 match {
    case True => Right(True)
    case False if v2 == Right(True) => Right(True)
    case False if v2 == Right(False) => Right(False)
    case _ => Left(Errors.binaryException("Bad Types"))
  }
}

case object LazyBAD extends LazyBinaryOp {
  def apply(v1: Value, v2: => Val): Val = Left(Errors.binaryException("LazyBAD operator"))
}

object LazyBinaryOp {
  implicit def str2lazyBinOp(repr: String): LazyBinaryOp = repr match {
        case "&&" => #&&#
        case "||" => #||#
    case _ => LazyBAD
  }
}

sealed abstract class UnaryOp {
  def apply(v: Value): Val
}

case object MINUS extends UnaryOp {
  def apply(v: Value): Val = v match {
    case IntNumber(value) => Right(IntNumber(-value))
    case RealNumber(value) => Right(RealNumber(-value))
    case _ => Left(Errors.binaryException("Bad type in unary expression"))
  }
}

case object NOT extends UnaryOp {
  def apply(v: Value): Val = v match {
    case True => Right(False)
    case False => Right(True)
    case _ => Left(Errors.binaryException("Bad type in unary expression"))
  }
}




object UnaryOp {
  implicit def str2unOp(repr: String): UnaryOp = repr match {
    case "-" => MINUS
    case "not" => NOT
  }
}