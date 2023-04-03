package io.overrider

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class OverriderGenericTraitSpec extends AnyFreeSpec with Matchers {
  trait Ctx
  object Ctx {
    implicit val ctx: Ctx = null
  }
  trait Sample[T] {
    val l1: List[T]
    def f1(s: String)(implicit ctx: Ctx): Int
    def f11(s1: String)(i1: T): String
    def f2(s1: String, i1: Int): T
  }
  object Sample extends Sample[Int] {
    override val l1: List[Int] = List(0)
    override def f1(s: String)(implicit ctx: Ctx): Int = 0
    override def f11(s1: String)(i1: Int): String = ""
    override def f2(s1: String, i1: Int): Int = 0
  }

  val s1 =
    MethodOverrider.withOverrides[Sample[Int]](Sample)(
      Overrides[Sample[Int]]
        .define(_.l1)(List(1, 2))
        .define(x => (y: String) => x.f1(y)(_: Ctx))(i => _ => i.toInt)
        .define(x => x.f2 _)((s, i) => { s.toInt + i })
        .define(x => x.f11 _)(s => i => s ++ i.toString)
    )

  val s2: Sample[Int] =
    MethodOverrider.withOverrides[Sample[Int]](Sample)(Overrides[Sample[Int]])

  trait Algebra[F[_]] {
    def pure(i: Int): F[Int]
  }

  object Algebra extends Algebra[List] {
    override def pure(i: Int): List[Int] = Nil
  }

  val a1: Algebra[List] = MethodOverrider.withOverrides[Algebra[List]](Algebra)(
    Overrides[Algebra[List]]
      .define(x => x.pure _)(List(_))
  )

  val a2: Algebra[List] = MethodOverrider.withOverrides[Algebra[List]](Algebra)(
    Overrides[Algebra[List]]
  )

  trait WithAlias {
    type T
    type F[A]

    val value: T

    def wrap(t: T): F[T]
    def extract(t: F[T]): T
    def traverse(t: List[F[T]]): F[List[T]]
  }

  type WithAliasIntOption = WithAlias {
    type T = Int
    type F[A] = Option[A]
  }

  object WithAlias extends WithAlias {
    override type T = Int
    override type F[A] = Option[A]

    override val value: T = 0

    override def wrap(t: T): F[T] = Some(t)
    override def extract(t: F[T]): T = t.get
    override def traverse(t: List[F[T]]): F[List[T]] =
      if (t.forall(_.isDefined)) Some(t.map(_.get))
      else None
  }

  val wa1: WithAliasIntOption = MethodOverrider.withOverrides[WithAlias](WithAlias)(Overrides[WithAlias])

  //val withAliasAliased: WithAlias = WithAlias
  //val wa2 = MethodOverrider.withOverrides[WithAlias](withAliasAliased)(Overrides[WithAlias])

  "Overrides" - {
    "properly overrides methods which refers trait type param" in {
      s1.l1 mustBe List(1, 2)
      s1.f1("12") mustBe 12
      s1.f2("2", 1) mustBe 3
      s1.f11("a")(1) mustBe "a1"
    }

    "properly delegates methods which refers trait type param" in {
      s2.l1 mustBe List(0)
      s2.f1("12") mustBe 0
      s2.f2("a", 1) mustBe 0
      s2.f11("a")(1) mustBe ""
    }

    "properly overrides methods with higher kinds types" in {
      a1.pure(42) mustBe List(42)
    }

    "properly delegates methods with higher kinds types" in {
      a2.pure(42) mustBe Nil
    }

    "properly delegates methods based on type aliases" in {
      wa1.wrap(1) mustBe Some(1)
      wa1.extract(Some(2)) mustBe 2
      wa1.traverse(List(Some(1), Some(2))) mustBe Some(List(1, 2))
      wa1.traverse(List(Some(1), None)) mustBe None
    }

    //WIP
    //"properly delegates methods based on type aliases abstract types" in {
    //  wa2.wrap(wa2.value) mustBe Some(wa2.value)
    //  wa2.extract(wa2.wrap(wa2.value)) mustBe wa2.value
    //}
  }
}
