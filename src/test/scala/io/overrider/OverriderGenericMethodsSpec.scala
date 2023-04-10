package io.overrider

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class OverriderGenericMethodsSpec extends AnyFreeSpec with Matchers {

  trait Generic {
    def generic[A](a: A): String
    def generic2[A](a: A): (A, String)

    def genericHigher[F[_], A](f: F[A]): F[A]

    def genericList[A](f: List[(A, Int)]): List[A]
  }

  object Generic extends Generic {
    override def generic[A](a: A): String = ""

    override def generic2[A](a: A): (A, String) = a -> generic(a)

    override def genericHigher[F[_], A](f: F[A]): F[A] = f

    override def genericList[A](f: List[(A, Int)]): List[A] = Nil
  }

  val g1 = MethodOverrider.withOverrides[Generic](Generic)(Overrides[Generic])

  "Overrides" - {
    "properly delegates generic methods" in {
      g1.generic(123) mustBe ""
      g1.genericHigher(List(1)) mustBe List(1)
    }
  }
}
