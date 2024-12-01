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
    assertEquals(CalcOps("unknownConst"), "unhandled input: unknownConst")
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
}
