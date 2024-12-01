package dodropin

class CalcSpec extends munit.FunSuite {
  test("empty stack reports error") {
    assertEquals(CalcOps(""), "error: no equation to evaulate")
  }

  test("calc 1 returns 1") {
    assertEquals(CalcOps("1"), "1")
  }

  test("calc 1 + 2 returns 3") {
    assertEquals(CalcOps("1 + 2"), "3")
  }

  test("calc 1 + returns err") {
    assertEquals(CalcOps("1 +"), "error: could not evaluate: + using: '1'")
  }

  test("calc 4 - 3 returns 1") {
    assertEquals(CalcOps("4 - 3"), "1")
  }

  test("calc 4 * 3 returns 12") {
    assertEquals(CalcOps("4 * 3"), "12")
  }

  test("calc 12 / 3 returns 4") {
    assertEquals(CalcOps("12 / 3"), "4")
  }

  test("calc 4 * 3.0 returns 12.0") {
    assertEquals(CalcOps("4 * 3.0"), "12.0")
  }

  test("calc 12 / 3.0 returns 4") {
    assertEquals(CalcOps("12 / 3.0"), "4")
  }

  test("calc 3 + 2 + 5 returns 10") {
    assertEquals(CalcOps("3 + 2 + 5"), "10")
  }

  test("calc 3 * 2 + 5 returns 11") {
    assertEquals(CalcOps("3 * 2 + 5"), "11")
  }

  test("calc 1 - 3 * 2 + 5 returns -10") {
    assertEquals(CalcOps("1 - 3 * 2 + 5"), "-10")
  }

  test("calc 3*(2 + 5) returns 21") {
    assertEquals(CalcOps("3*(2 + 5)"), "21")
  }

  test("calc 3*{ 2 + 5 } returns 21") {
    assertEquals(CalcOps("3*{ 2 + 5 }"), "21")
  }

  test("calc 3*[ 2 + 5 ] returns 21") {
    assertEquals(CalcOps("3*[ 2 + 5 ]"), "21")
  }

  test("calc 2^5 returns 32") {
    assertEquals(CalcOps("2^5"), "32")
  }

  test("calc 2^5.0 returns 32.0") {
    assertEquals(CalcOps("2^5.0"), "32.0")
  }

  test("calc (3-1)^5 returns 32") {
    assertEquals(CalcOps("(3-1)^5"), "32")
  }

  test("calc 3+2*(3-1)^5+6*7 returns 109") {
    assertEquals(CalcOps("3+2*(3-1)^5+6*7"), "109")
  }

  test("calc unknownConst returns error") {
    assertEquals(CalcOps("unknownConst"), "error: unhandled input: unknownConst")
  }

  test("calc const pi returns 3.14... ") {
    assertEquals(CalcOps("pi").take(6), "3.1415")
  }

  test("calc const e returns 2.718... ") {
    assertEquals(CalcOps("e").take(5), "2.718")
  }

  test("calc 113 * pi returns 355.0 ") {
    assertEquals(CalcOps("113 * pi").take(6), "355.00")
  }

  test("calc pi * (3 + 110) returns 355.0 ") {
    assertEquals(CalcOps("pi * (3 + 110)").take(5), 355.0.toString)
  }

  test("calc sqr 2 returns 4 ") {
    assertEquals(CalcOps("sqr 2"), "4")
  }

  test("calc sqr 3.5 returns 12.25 ") {
    assertEquals(CalcOps("    sqr 3.5 "), "12.25")
  }

  test("calc sqrt 4 returns 2.0 ") {
    assertEquals(CalcOps("sqrt 4"), "2.0")
  }

  test("calc sqrt 12.25 returns 3.5 ") {
    assertEquals(CalcOps("    sqrt 12.25 "), "3.5")
  }

  test("calc sin(pi / 2) returns 1 ") {
    assertEquals(CalcOps("    sin(pi / 2)").take(7), "0.99999")
  }

  test("calc cos pi returns 1 ") {
    assertEquals(CalcOps("    cos pi ").take(7), "-0.9999")
  }

  test("calc tan  pi / 4 returns 1.0 ") {
    assertEquals(CalcOps("    tan  pi / 4 ").take(7), "1.00000")
  }
}
