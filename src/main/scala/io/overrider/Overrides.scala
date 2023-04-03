package io.overrider

/** Fake type to define method overrides.
  * Shouldn't be used outside of MethodOverrider.withOverrides argument list, as it doesn't represent any real value
  * but it's tree just allows to define multiple method overrides
  * @tparam T Type of object which method we try to overwrite
  */
sealed trait Overrides[T] {

  /** Defines overwrite of single method
    * @param selector method invocation
    * @param replacer value which should be used in overwrite
    * @tparam R Method signature
    * @return Overrides object
    */
  def define[R](selector: T => R)(replacer: R): Overrides[T]
}

object Overrides {
  def apply[T]: Overrides[T] = null
}
