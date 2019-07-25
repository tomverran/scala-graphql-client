package io.tvc.graphql

import cats.data.NonEmptyList
import cats.syntax.functor._
import io.circe.Decoder
import io.circe.generic.semiauto._

/**
  * All calls to GraphQL servers should expect this result back
  * https://graphql.github.io/graphql-spec/June2018/#sec-Errors-and-Non-Nullability
  */
sealed trait Response[+A]

object Response {

  private implicit def optionalList[A: Decoder]: Decoder[List[A]] =
    Decoder.decodeOption(Decoder.decodeList(Decoder[A])).map(_.toList.flatten)

  case class Location(line: Int, column: Int)

  object Location {
    implicit val decoder: Decoder[Location] = deriveDecoder
  }

  case class Error(
    message: String,
    locations: List[Location],
    path: List[String]
  )

  object Error {
    implicit val decoder: Decoder[Error] = deriveDecoder
  }

  case class Success[A](data: A, errors: List[Error]) extends Response[A]
  case class Failure(errors: NonEmptyList[Error]) extends Response[Nothing]

  implicit def decoder[A: Decoder]: Decoder[Response[A]] =
    deriveDecoder[Success[A]].widen[Response[A]].or(deriveDecoder[Failure].widen)
}
