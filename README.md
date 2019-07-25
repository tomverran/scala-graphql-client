## Sadly nowhere near ready yet

The idea is this'll be a Scala GraphQL client that uses code generation (remember that?!) to
produce strongly typed GraphQL calls. I can't really see how else you'd do it given
that GraphQL lets you select as much or as little from a type as you want.

Currently there are parsers for queries and (most) of the parts of a schema,
the idea is then to write code to stitch the resulting data together to produce scala files
containing the unique types used by each GraphQL query.

Currently the example GitHub query in `src/main/resources/query.graphql` produces the following:


```scala
package com.github.api
import io.tvc.graphql.Runtime._
import io.circe.generic.JsonCodec
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
  case class Organization(
    repo1: Option[Repository],
    repo2: Option[Repository],
    repo3: Option[Repository],
    repo4: Option[Repository]
  )
  
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
  case class Query(organization: Option[Organization])
  
  sealed trait MergeableState extends EnumEntry
  
  object MergeableState extends GraphQLEnum[MergeableState] {
    case object CONFLICTING extends MergeableState
    case object MERGEABLE extends MergeableState
    case object UNKNOWN extends MergeableState
    val values = findValues
  }
  
  val query: String = 
    """
    |query foo {
    |    organization(login: "blah") {
    |        repo1: repository(name: "repo-one") {
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
    |        repo2: repository(name: "repo-two") {
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
    |        repo3: repository(name: "repo-three") {
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
    |        repo4: repository(name: "repo-four") {
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
    """.stripMargin
}
```
