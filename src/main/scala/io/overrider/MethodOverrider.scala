package io.overrider

import scala.language.experimental.macros

//noinspection ScalaUnusedSymbol
object MethodOverrider {
  def withOverrides[A](original: A)(overrides: Overrides[A]): A =
    macro MethodOverriderImpl.withOverrides[A]

  def printer(a: Any): Unit = macro MethodOverriderImpl.print

}
