package ru.spbau.osipov.inter.errors

/**
 * @author stasstels
 * @since 1/23/14.
 */
object Errors {
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
}
