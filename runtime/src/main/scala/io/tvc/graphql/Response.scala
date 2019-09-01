package io.tvc.graphql

import cats.MonadError
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

  /**
    * Decoder that decodes a list but returns an empty list if
    * the field doesn't exist rather than throwing an error
    */
  implicit def optionalList[A: Decoder]: Decoder[List[A]] =
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

  implicit class ResponseOps[A](response: Response[A]) {

    /**
      * Convert the Response into an either.
      * We convert successes into failures if they have any errors at all
      */
    def either: Either[NonEmptyList[Error], A] =
      response match {
        case Failure(errors) => Left(errors)
        case Success(_, er :: errors) => Left(NonEmptyList(er, errors))
        case Success(data, Nil) => Right(data)
      }

    /**
      * Convert the response into an effect type F
      * by putting any errors into an exception
      */
    def value[F[_]](implicit F: MonadError[F, Throwable]): F[A] =
      F.fromEither(either.left.map(errors => new Exception(errors.toList.mkString(", "))))
  }

}
