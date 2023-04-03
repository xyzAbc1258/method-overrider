package io.overrider

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class OverriderGenericMethodsSpec extends AnyFreeSpec with Matchers {

  trait Generic {
    def generic[A](a: A): String

    def genericHigher[F[_], A](f: F[A]): F[A]
  }

  object Generic extends Generic {
    override def generic[A](a: A): String = ""

    override def genericHigher[F[_], A](f: F[A]): F[A] = f
  }

  val g1 = MethodOverrider.withOverrides[Generic](Generic)(Overrides[Generic])

  "Overrides" - {
    "properly delegates generic methods" in {
      g1.generic(123) mustBe ""
      g1.genericHigher(List(1)) mustBe List(1)
    }
  }
}
