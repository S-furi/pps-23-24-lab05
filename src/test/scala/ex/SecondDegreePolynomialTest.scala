package ex

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class SecondDegreePolynomialTest:
  val simplePolynomial = SecondDegreePolynomial(1.0, 0, 3)
  val anotherPolynomial = SecondDegreePolynomial(0.0, 1, 0.0)
  val fullPolynomial = SecondDegreePolynomial(3.0, 2.0, 5.0)

  @Test def testEqualityWorks(): Unit = {
    assertEquals(simplePolynomial, simplePolynomial)
  }

  @Test def testSum(): Unit = {
    val sum = simplePolynomial + anotherPolynomial
    assertEquals(
      SecondDegreePolynomial(1.0, 1.0, 3.0),
      sum
    )
  }

  @Test def testMultipleOperations(): Unit = {
    val sum = fullPolynomial - (anotherPolynomial + simplePolynomial)
    assertEquals(
      SecondDegreePolynomial(2.0, 1.0, 2.0),
      sum
    )
  }