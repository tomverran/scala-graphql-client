package io.tvc.graphql.parsing

import atto.Parser
import atto.parser.character.char
import atto.parser.combinator._
import atto.syntax.parser._
import cats.data.NonEmptyList
import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import cats.syntax.apply._
import cats.syntax.functor._
import io.tvc.graphql.parsing.Combinators._
import io.tvc.graphql.parsing.CommonModel._
import io.tvc.graphql.parsing.CommonParser._
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel._

object SchemaParser {

  val description: Parser[Option[Description]] =
    ws(opt(validString.map(s => Description(s.trim))))

  private val scalarTypeDefinition: Parser[ScalarTypeDefinition] =
    (description <~ str("scalar"), name, directives).mapN(ScalarTypeDefinition)

  private val enumValueDefinitions: Parser[List[EnumValueDefinition]] =
    recCurlyBrackets(many((description, enumValue, directives).mapN(EnumValueDefinition)))

  private val enumTypeDefinition: Parser[EnumTypeDefinition] =
    (description <~ str("enum"), name, directives, enumValueDefinitions).mapN(EnumTypeDefinition)

  private val unionMemberTypes: Parser[List[Name]] =
    ws(char('=')) ~> separatedWith(name, '|')

  private val unionTypeDefinition: Parser[UnionTypeDefinition] =
    (description <~ str("union"), name, directives, unionMemberTypes).mapN(UnionTypeDefinition)

  private val inputValueDefinition: Parser[InputValueDefinition] =
    (description, name <~ colon, `type`, opt(ws(char('=')) ~> value), directives).mapN(InputValueDefinition)

  private val argumentsDefinition: Parser[List[InputValueDefinition]] =
    ws(optRecParens(many(inputValueDefinition)).map(_.flatten))

  private val fieldDefinition: Parser[FieldDefinition] =
    (description, name, argumentsDefinition <~ colon, `type`, directives).mapN(FieldDefinition)

  private val fieldsDefinition: Parser[List[FieldDefinition]] =
    recCurlyBrackets(many(fieldDefinition).map(_.toList))

  private val interfaceTypeDefinition: Parser[InterfaceTypeDefinition] =
    (description <~ str("interface"), name, directives, fieldsDefinition).mapN(InterfaceTypeDefinition)

  private val implements: Parser[List[Name]] =
    opt(str("implements") ~> separatedWith(name, '&')).map(_.toList.flatten)

  private val objectTypeDefinition: Parser[ObjectTypeDefinition] =
    (description <~ str("type"), name, implements, directives, fieldsDefinition).mapN(ObjectTypeDefinition)

  private val inputFieldsDefinition: Parser[List[InputValueDefinition]] =
    recCurlyBrackets(many(inputValueDefinition))

  private val inputObjectTypeDefinition: Parser[InputObjectTypeDefinition] =
    (description <~ str("input"), name, directives, inputFieldsDefinition).mapN(InputObjectTypeDefinition)

  private val rootOperationTypeDefinition: Parser[RootOperationTypeDefinition] =
    (ws(operationType) <~ ws(colon), name).mapN(RootOperationTypeDefinition.apply)

  private val schemaDefinition: Parser[SchemaDefinition] =
    str("schema") ~> recCurlyBrackets(many1(rootOperationTypeDefinition)).map(SchemaDefinition.apply)

  private val typeDefinition: Parser[TypeDefinition] =
    ws(
      choice[TypeDefinition](
        scalarTypeDefinition.widen,
        enumTypeDefinition.widen,
        unionTypeDefinition.widen,
        interfaceTypeDefinition.widen,
        objectTypeDefinition.widen,
        inputObjectTypeDefinition.widen
      ) | get.flatMap(txt => err(s"Expected type definition, got: ${txt.take(30)}..."))
    )

  private val definitions: Parser[NonEmptyList[Either[TypeDefinition, SchemaDefinition]]] =
    many1(either(ws(typeDefinition), ws(schemaDefinition))) |
    get.flatMap(txt => err(s"Expected type or schema, got ${txt.take(30)}..."))

  def parse(string: String): Either[String, Schema] =
    definitions.map { items =>
      val (types, schemas) = items.toList.separate
      Schema(schemas.headOption, types.groupBy(_.name).mapValues(_.head))
    }.parseOnly(string).either
}
