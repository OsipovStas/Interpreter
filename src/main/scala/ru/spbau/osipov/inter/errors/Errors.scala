package ru.spbau.osipov.inter.errors

/**
 * @author stasstels
 * @since 1/23/14.
 */
object Errors {
  type Errors = Seq[String]

  def noDefFound(name: String) = Seq(s"Can't find definition of variable $name")

  def wrongParameters(expected: String, actual: String) = Seq(
    s"""
      |Wrong parameters number:
      |Expected: $expected
      |Actual: $actual
    """.stripMargin
  )

  def functionExpected(actual: String) = Seq(
    s"""
      |Function value expected
      |Actual: $actual
    """.stripMargin
  )

  def structureExpected(actual: String) = Seq(
    s"""
      |Structure value expected
      |Actual: $actual
    """.stripMargin
  )

  def binaryException(what: String) = Seq(
    s"""
      |Exception in binary expression:
      |Msg: $what
    """.stripMargin
  )

  def expectedLogicValue(actual: String) = Seq(
    s"""
      |Expected logic value
      |Actual: $actual
    """.stripMargin
  )
}
