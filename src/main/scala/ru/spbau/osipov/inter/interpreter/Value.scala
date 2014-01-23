package ru.spbau.osipov.inter.interpreter

import ru.spbau.osipov.inter.Interpreter._


/**
 * @author stasstels
 * @since 1/18/14.
 */
sealed abstract class Value

abstract class Number extends Value {
  def getNumber: BigDecimal
}

case class IntNumber(value: BigInt) extends Number {
  def getNumber: BigDecimal = BigDecimal(value)
}

case class RealNumber(value: BigDecimal) extends Number {
  def getNumber: BigDecimal = value
}

abstract class Logic(value: Boolean) extends Value

case object True extends Logic(true)

case object False extends Logic(false)

case class Chars(value: String) extends Value

case class Function(bindings: Seq[Var], body: Node, scope: Ctx) extends Value

case class Structure(data: Map[String, Value]) extends Value

case object Single extends Value