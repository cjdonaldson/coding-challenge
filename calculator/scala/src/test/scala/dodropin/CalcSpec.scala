package dodropin

class CalcSpec extends munit.FunSuite {
  test("calc 1 returns 1") {
    assertEquals(dodropin.CalcOps.apply("1"), "1")
  }

  test("calc 1 + 2 returns 3") {
    assertEquals(dodropin.CalcOps.apply("1 + 2"), "3")
  }
}
