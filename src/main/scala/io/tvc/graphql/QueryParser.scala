package io.tvc.graphql

import atto.Parser
import atto.parser.character._
import atto.parser.combinator._
import atto.syntax.parser._
import cats.syntax.apply._
import cats.syntax.functor._
import io.tvc.graphql.Combinators._
import io.tvc.graphql.CommonModel._
import io.tvc.graphql.CommonParser._
import io.tvc.graphql.QueryModel.OperationType.Query
import io.tvc.graphql.QueryModel._

object QueryParser {

  val operationType: Parser[OperationType] =
    name.flatMap {
      case Name("query") => ok(OperationType.Query)
      case Name("mutation") => ok(OperationType.Mutation)
      case Name("subscription") => ok(OperationType.Subscription)
      case Name(n) => err(s"Expected one of query/mutation/subscription, got $n")
    }

  val variableDefinition: Parser[VariableDefinition] =
    (
      (variable <~ ws(char(':'))) ~ ws(`type`) ~ (ws(char('=')) ~> opt(value))
    ).map { case ((v, t), d) => VariableDefinition(v, t, d) }

  lazy val field: Parser[Field] =
    rec((opt(name <~ ws(char(':'))), name, argumentList, opt(selectionSet)).mapN(Field.apply))

  val selectionSet: Parser[SelectionSet] =
    recCurlyBrackets(many(ws(field))).map(SelectionSet.apply)

  val operationDefinition: Parser[OperationDefinition] =
    (
      opt(operationType).map(_.getOrElse(Query)),
      ws(opt(name)),
      optRecParens(commaSeparated(variableDefinition)).map(_.flatten),
      selectionSet
    ).mapN(OperationDefinition.apply)
}
