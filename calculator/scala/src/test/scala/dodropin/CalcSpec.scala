package dodropin

class CalcSpec extends munit.FunSuite {
  test("calc 1 returns 1") {
    assertEquals(dodropin.CalcOps.apply("1"), "1")
  }

  test("calc 1 + 2 returns 3") {
    assertEquals(dodropin.CalcOps.apply("1 + 2"), "3")
  }

  test("calc 4 - 3 returns 1") {
    assertEquals(dodropin.CalcOps.apply("4 - 3"), "1")
  }

  test("calc 4 * 3 returns 12") {
    assertEquals(dodropin.CalcOps.apply("4 * 3"), "12")
  }

  test("calc 12 / 3 returns 4") {
    assertEquals(dodropin.CalcOps.apply("12 / 3"), "4")
  }

  test("calc 4 * 3.0 returns 12.0") {
    assertEquals(dodropin.CalcOps.apply("4 * 3.0"), "12.0")
  }

  test("calc 12 / 3.0 returns 4.0") {
    assertEquals(dodropin.CalcOps.apply("12 / 3.0"), "4.0")
  }

  test("calc 3 + 2 + 5 returns 10") {
    assertEquals(dodropin.CalcOps.apply("3 + 2 + 5"), "10")
  }
}
