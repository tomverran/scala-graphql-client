package io.tvc.graphql

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto._

case class Request[A](query: String, variables: Json = Json.obj())

object Request {
  implicit def encoder[A]: Encoder[Request[A]] = deriveEncoder
  implicit def decoder[A]: Decoder[Request[A]] = deriveDecoder
}




