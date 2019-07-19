package io.tvc.graphql.parsing

import atto.Parser
import atto.parser.character.char
import atto.parser.combinator._
import atto.syntax.parser._
import cats.syntax.apply._
import cats.syntax.functor._
import io.tvc.graphql.parsing.Combinators._
import io.tvc.graphql.parsing.CommonModel._
import io.tvc.graphql.parsing.CommonParser._
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition._
import io.tvc.graphql.parsing.SchemaModel._

import scala.io.Source

object SchemaParser {

  def load(file: String): String = {
    val source = Source.fromURL(getClass.getResource(file))
    val lines = source.getLines.mkString("\n")
    source.close
    lines
  }

  val description: Parser[Option[Description]] =
    ws(opt(validString.map(Description)))

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

  private val typeDefinition: Parser[TypeDefinition] =
    choice[TypeDefinition](
      scalarTypeDefinition.widen,
      enumTypeDefinition.widen,
      unionTypeDefinition.widen,
      interfaceTypeDefinition.widen,
      objectTypeDefinition.widen,
      inputObjectTypeDefinition.widen
    )

  val schema: Parser[Schema] =
    many(typeDefinition)

  //high quality production ready testing system
//  println(QueryParser.operationDefinition.parseOnly(load("/queries/query.graphql")).done)
//  println(scalarTypeDefinition.parseOnly("\"blah\" scalar Foo @bar(reason: \"why\") @baz(what: 2)   "))
//  println(enumTypeDefinition.parseOnly(load("/schemas/enum.idl")).done)
//  println(unionTypeDefinition.parseOnly(load("/schemas/union.idl")).done)
//  println(interfaceTypeDefinition.parseOnly(load("/schemas/interface.idl")).done)
//  println(objectTypeDefinition.parseOnly(load("/schemas/object.idl")).done)
//  println(inputObjectTypeDefinition.parseOnly(load("/schemas/inputobject.idl")).done)
//  println(schema.parseOnly(load("/schemas/schema.idl")).done)
}
