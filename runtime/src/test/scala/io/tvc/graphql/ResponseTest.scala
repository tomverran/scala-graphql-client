package io.tvc.graphql

import cats.data.NonEmptyList
import io.circe.generic.JsonCodec
import org.scalatest.{Matchers, WordSpec}
import io.circe.parser.decode
import io.tvc.graphql.Response.{Location, Success}
import cats.instances.either._

class ResponseTest extends WordSpec with Matchers {

  @JsonCodec
  case class RepositoryConnection(totalCount: Int)

  @JsonCodec
  case class Organization(repositories: RepositoryConnection)

  @JsonCodec
  case class Output(organization: Option[Organization])

  type OrError[A] = Either[Throwable, A]

  "Response" should {

    "Be able to decode a total failure from Github" in {

      val errors: String =
        """
          |{
          |  "errors" : [
          |    {
          |      "path" : [
          |        "query foo",
          |        "organization",
          |        "repositories",
          |        "name"
          |      ],
          |      "extensions" : {
          |        "code" : "argumentNotAccepted",
          |        "name" : "repositories",
          |        "typeName" : "Field",
          |        "argumentName" : "name"
          |      },
          |      "locations" : [
          |        {
          |          "line" : 3,
          |          "column" : 22
          |        }
          |      ],
          |      "message" : "Field 'repositories' doesn't accept argument 'name'"
          |    },
          |    {
          |      "path" : [
          |        "query foo"
          |      ],
          |      "extensions" : {
          |        "code" : "variableNotUsed",
          |        "variableName" : "repository"
          |      },
          |      "locations" : [
          |        {
          |          "line" : 1,
          |          "column" : 1
          |        }
          |      ],
          |      "message" : "Variable $repository is declared by foo but not used"
          |    }
          |  ]
          |}
        """.stripMargin


      decode[Response[Output]](errors) shouldBe Right(
        Response.Failure(
          errors = NonEmptyList.of(
            Response.Error(
              message = "Field 'repositories' doesn't accept argument 'name'",
              path = List("query foo", "organization", "repositories", "name"),
              locations = List(Location(3, 22))
            ),
            Response.Error(
              message = "Variable $repository is declared by foo but not used",
              locations = List(Location(1, 1)),
              path = List("query foo")
            )
          )
        )
      )
    }

    "Be able to decode a partial failure from Github" in {

      val error: String =
        """
          |{
          |  "data" : {
          |    "organization" : null
          |  },
          |  "errors" : [
          |    {
          |      "type" : "NOT_FOUND",
          |      "path" : [
          |        "organization"
          |      ],
          |      "locations" : [
          |        {
          |          "line" : 2,
          |          "column" : 5
          |        }
          |      ],
          |      "message" : "Could not resolve to an Organization with the login of 'foo'."
          |    }
          |  ]
          |}
        """.stripMargin

      decode[Response[Output]](error) shouldBe Right(
        Success(
          Output(None),
          errors = List(
            Response.Error(
              message = "Could not resolve to an Organization with the login of 'foo'.",
              locations = List(Location(2, 5)),
              path = List("organization")
            )
          )
        )
      )
    }

    "Be able to decode a success from Github" in {

      val success: String =
        """
          |{
          |  "data" : {
          |    "organization" : {
          |      "repositories" : {
          |        "totalCount" : 1234
          |      }
          |    }
          |  }
          |}
        """.stripMargin

      decode[Response[Output]](success) shouldBe Right(
        Response.Success(
          errors = List.empty,
          data = Output(
            Some(
              Organization(
                RepositoryConnection(
                  totalCount = 1234
                )
              )
            )
          )
        )
      )
    }

    "Turn a success into a Right if it has no errors" in {
      Response.Success((), List.empty).either shouldBe Right(())
    }

    "Turn a success into a Left if it has any errors" in {
      val error = Response.Error("foo", List.empty, List.empty)
      Response.Success((), List(error)).either shouldBe Left(NonEmptyList.of(error))
    }

    "Turn a failure into a Left" in {
      val error = Response.Error("foo", List.empty, List.empty)
      Response.Failure(NonEmptyList.of(error)).either shouldBe Left(NonEmptyList.of(error))
    }

    "Turn a failure into a failed effect" in {
      val error = Response.Error("foo", List.empty, List.empty)
      Response.Failure(NonEmptyList.of(error)).value[OrError]should matchPattern { case Left(_) => }
    }
  }
}
