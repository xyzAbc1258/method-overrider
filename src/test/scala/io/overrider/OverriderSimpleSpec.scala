package io.overrider

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class OverriderSimpleSpec extends AnyFreeSpec with Matchers {
  trait Ctx
  object Ctx {
    implicit val ctx: Ctx = null
  }
  trait Sample {
    val i: Int
    def s: String
    def f1(s: String)(implicit ctx: Ctx): Int
    def f11(s1: String)(i1: Int): String
    def f2(s1: String, i1: Int): String
  }
  object Sample extends Sample {
    override val i: Int = 0
    override def s: String = ""
    override def f1(s: String)(implicit ctx: Ctx): Int = 0
    override def f11(s1: String)(i1: Int): String = ""
    override def f2(s1: String, i1: Int): String = ""
  }

  val s1: Sample =
    MethodOverrider.withOverrides[Sample](Sample)(
      Overrides[Sample]
        .define(_.i)(1)
        .define(_.s)("1")
        .define(x => (y: String) => x.f1(y)(_: Ctx))(i => _ => i.toInt)
        .define(x => x.f2 _)((s, i) => { s ++ i.toString })
        .define(x => x.f11 _)(s => i => s ++ i.toString)
    )

  val s2: Sample = MethodOverrider.withOverrides[Sample](Sample)(Overrides[Sample])

  "Overrides" - {
    "properly overrides simple methods" in {
      s1.i mustBe 1
      s1.s mustBe "1"
      s1.f1("12") mustBe 12
      s1.f2("a", 1) mustBe "a1"
      s1.f11("a")(1) mustBe "a1"
    }

    "properly delegates simple methods" in {
      s2.i mustBe 0
      s2.s mustBe ""
      s2.f1("12") mustBe 0
      s2.f2("a", 1) mustBe ""
      s2.f11("a")(1) mustBe ""
    }
  }
}
