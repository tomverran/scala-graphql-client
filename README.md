## Sadly nowhere near ready yet

The idea is this'll be a Scala GraphQL client that uses code generation (remember that?!) to
produce strongly typed GraphQL calls. I can't really see how else you'd do it given
that GraphQL lets you select as much or as little from a type as you want.

Remaining parts of the library to implement before release are

- Union types
- Default input values

Currently the example GitHub query in `src/main/resources/query.graphql` produces the following:

```scala
package com.github.api
import io.tvc.graphql.Builtin._
import io.tvc.graphql.Request
import io.tvc.graphql.Response
import io.circe.generic.JsonCodec
import io.circe.syntax._
import enumeratum._

object Foo {

  /**
    * A repository contains the content for a project.
    */
  @JsonCodec
  case class Repository(pullRequests: PullRequestConnection)
  
  /**
    * A repository pull request.
    */
  @JsonCodec
  case class PullRequest(
    author: Option[Actor],
    mergeable: MergeableState,
    number: Int,
    title: String,
    body: String
  )
  
  /**
    * An account on GitHub, with one or more owners, that has repositories, members and teams.
    */
  @JsonCodec
  case class Organization(repo1: Option[Repository])
  
  /**
    * An edge in a connection.
    */
  @JsonCodec
  case class PullRequestEdge(node: Option[PullRequest], cursor: String)
  
  /**
    * Represents an object which can take actions on GitHub. Typically a User or Bot.
    */
  @JsonCodec
  case class Actor(login: String)
  
  /**
    * The connection type for PullRequest.
    */
  @JsonCodec
  case class PullRequestConnection(edges: Option[List[Option[PullRequestEdge]]])
  
  /**
    * The query root of GitHub's GraphQL interface.
    */
  @JsonCodec
  case class Output(organization: Option[Organization])
  
  @JsonCodec
  case class Variables(organization: String, repository: String)
  
  sealed trait MergeableState extends EnumEntry
  
  object MergeableState extends GraphQLEnum[MergeableState] {
    case object CONFLICTING extends MergeableState
    case object MERGEABLE extends MergeableState
    case object UNKNOWN extends MergeableState
    val values = findValues
  }
  
  def apply(organization: String, repository: String): Request[Response[Output]] =
    Request[Response[Output]](
      query = """
      |query foo($organization:String! $repository:String!) {
      |    organization(login: $organization) {
      |        repo1: repository(name: $repository) {
      |            pullRequests(states: OPEN, first: 100) {
      |                edges {
      |                    node {
      |                        author {
      |                            login
      |                        }
      |                        mergeable
      |                        number
      |                        title
      |                        body
      |                    }
      |                    cursor
      |                }
      |            }
      |        }
      |    }
      |}
      """.trim.stripMargin,
      variables = Variables(organization, repository).asJson
    )
}
```
