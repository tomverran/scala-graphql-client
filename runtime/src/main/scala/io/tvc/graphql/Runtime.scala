package io.tvc.graphql

import enumeratum._

/**
  * This object is imported into all generated files,
  * unfortunately alongside some other things hard coded in the generator code
  */
object Runtime {
  trait GraphQLEnum[A <: EnumEntry] extends Enum[A] with CirceEnum[A]
}
