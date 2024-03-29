package io.tvc.graphql

import enumeratum._
import io.circe.{Decoder, Encoder}

/**
  * This object is imported into all generated files,
  * unfortunately alongside some other things hard coded in the generator code
  */
object Builtin {

  trait GraphQLEnum[A <: EnumEntry] extends Enum[A] with CirceEnum[A]
  case class ID(value: String) extends AnyVal

  object ID {
    implicit val decoder: Decoder[ID] = Decoder.decodeString.map(ID.apply)
    implicit val encoder: Encoder[ID] = Encoder.encodeString.contramap(_.value)
  }
}
