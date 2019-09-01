package io.tvc.graphql.parsing

import atto.Parser
import atto.parser.character.char
import atto.parser.combinator.{many, opt}
import atto.syntax.parser._
import cats.syntax.apply._
import io.tvc.graphql.parsing.Combinators._
import io.tvc.graphql.parsing.CommonModel.OperationType.Query
import io.tvc.graphql.parsing.CommonParser._
import io.tvc.graphql.parsing.QueryModel._

object QueryParser {

  /**
    * We need to keep all the selection sets as a string
    * to later output them when generating code
    */
  case class ParsedQuery(
    operation: OperationDefinition,
    stringValue: String
  )



  private val variableDefinition: Parser[VariableDefinition] =
    (
      (variable <~ ws(char(':'))) ~ ws(`type`) ~ opt(ws(char('=')) ~> ws(value))
    ).map { case ((v, t), d) => VariableDefinition(v, t, d) }

  private lazy val field: Parser[Field] =
    rec((opt(name <~ ws(char(':'))), name, argumentList, opt(selectionSet)).mapN(Field.apply))

  private val selectionSet: Parser[SelectionSet] =
    recCurlyBrackets(many(ws(field))).map(SelectionSet.apply)

  private val operationDefinition: Parser[OperationDefinition] =
    (
      opt(operationType).map(_.getOrElse(Query)),
      ws(opt(name)),
      optRecParens(many(variableDefinition)).map(_.flatten),
      selectionSet
    ).mapN(OperationDefinition.apply)

  def parse(string: String): Either[String, OperationDefinition] =
    operationDefinition.parseOnly(string).either
}
