package io.tvc.graphql.parsing

import atto.Parser
import atto.parser.character.char
import atto.parser.combinator.{choice, many, opt}
import atto.parser.text.string
import atto.syntax.parser._
import cats.syntax.apply._
import cats.syntax.functor._
import io.tvc.graphql.parsing.Combinators._
import io.tvc.graphql.parsing.CommonModel.OperationType.Query
import io.tvc.graphql.parsing.CommonModel.Type.NamedType
import io.tvc.graphql.parsing.CommonParser._
import io.tvc.graphql.parsing.QueryModel.Selection.{FragmentSpread, InlineFragment}
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

  /**
    * VariableDefinition:
    * Variable: Type DefaultValueₒₚₜ
    */
  private val variableDefinition: Parser[VariableDefinition] =
    (
      (variable <~ ws(char(':'))) ~ ws(`type`) ~ opt(ws(char('=')) ~> ws(value))
    ).map { case ((v, t), d) => VariableDefinition(v, t, d) }

  /**
    * Field:
    * Aliasₒₚₜ Name Argumentsₒₚₜ Directivesₒₚₜ SelectionSetₒₚₜ
    */
  private lazy val field: Parser[Selection.Field] =
    rec((opt(name <~ ws(char(':'))), name, argumentList, opt(selectionSet)).mapN(Selection.Field.apply))

  /**
    * FragmentSpread:
    * ...FragmentName Directivesₒₚₜ
    */
  private val fragmentSpread: Parser[FragmentSpread] =
    ws(string("...")) ~> (name.filter(_.value != "on"), directives).mapN(FragmentSpread)

  /**
    * TypeCondition:
    * on NamedType
    */
  private val typeCondition: Parser[NamedType] =
    ws(string("on")) ~> name.map(NamedType)

  /**
    * InlineFragment:
    * ...TypeConditionₒₚₜ Directivesₒₚₜ SelectionSet
    */
  private lazy val inlineFragment: Parser[InlineFragment] =
    ws(string("...")) ~> (typeCondition, directives, selectionSet).mapN(InlineFragment)

  /**
    * Selection:
    * Field
    * FragmentSpread
    * InlineFragment
    */
  private val selection: Parser[Selection] =
    choice(field.widen, inlineFragment.widen, fragmentSpread.widen)

  /**
    * SelectionSet:
    * { Selection (list) }
    */
  private val selectionSet: Parser[SelectionSet] =
    recCurlyBrackets(many(ws(selection))).map(SelectionSet.apply)

  /**
    * FragmentDefinition:
    * fragment FragmentName TypeCondition Directivesₒₚₜ SelectionSet
    */
  private val fragmentDefinition: Parser[FragmentDefinition] =
    ws(string("fragment")) ~> (name, typeCondition, directives, selectionSet).mapN(FragmentDefinition)

  /**
    * OperationDefinition:
    * OperationType Nameₒₚₜ VariableDefinitionsₒₚₜ Directivesₒₚₜ SelectionSet
    */
  private val operationDefinition: Parser[OperationDefinition] =
    (
      opt(operationType).map(_.getOrElse(Query)),
      ws(opt(name)),
      optRecParens(many(variableDefinition)).map(_.flatten),
      selectionSet
    ).mapN(OperationDefinition.apply)

  /**
    * This doesn't actually match the spec
    * but I'm content to have one query per file
    */
  private val executableDefinition: Parser[ExecutableDefinition] =
    (operationDefinition, many(fragmentDefinition)).mapN(ExecutableDefinition)


  def parse(string: String): Either[String, ExecutableDefinition] =
    executableDefinition.parseOnly(string).either
}
