package io.tvc.graphql.generation

import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.foldable._
import cats.syntax.functor._
import io.tvc.graphql.generation.TypeDeduplicator.{FlatType, TypeRef}
import io.tvc.graphql.inlining.InputInliner.{InputObject, InputValue}
import io.tvc.graphql.inlining.TypeTree
import io.tvc.graphql.inlining.TypeTree.TypeModifier.{ListType, NonNullType, NullableType}
import io.tvc.graphql.inlining.TypeTree.{Scalar, TypeModifier}

object ScalaCodeGenerator {

  case class Query(
    name: String,
    operation: String,
    selectionSets: String,
    inputs: InputObject[TypeRef]
  )

  case class Generateable(
    namespace: String,
    types: List[FlatType],
    query: Query
  )

  private val indent: String = "  "

  private def indent(s: String): String =
    s.lines.map(l => if (l.isEmpty) "" else s"$indent$l").mkString("\n")

  private def blockComment(s: String): String =
    s.lines.toList.filter(_.nonEmpty).map(l => s"$indent* $l").foldSmash("/**\n", "\n", s"\n$indent*/\n")

  private def argList(as: List[String]): String =
    if (as.length < 3) as.foldSmash("(", ", ", ")") else as.foldSmash(s"(\n$indent", s",\n$indent", "\n)")

  private def curlyBlock(what: List[String]): String =
    what.foldSmash(s"{\n$indent", s"\n$indent", "\n}")

  private def fieldType(tpe: TypeRef, mods: List[TypeModifier]): String =
    mods.foldLeft(tpe.name) {
      case (n, NonNullType) => n
      case (n, ListType) => s"List[$n]"
      case (n, NullableType) => s"Option[$n]"
    }

  private def fields(fs: List[TypeTree.Field[Either[TypeRef, InputValue[TypeRef]]]]): String =
    argList(fs.map(f => s"${f.name.alias.getOrElse(f.name.value)}: ${fieldType(f.`type`.map(_.value).merge, f.modifiers)}"))

  private[generation] def caseClass(qr: TypeTree.Object[Either[TypeRef, InputValue[TypeRef]]]): String =
    s"${qr.meta.comment.foldMap(blockComment)}@JsonCodec\ncase class ${qr.meta.name}${fields(qr.fields)}" +
    s"\n\n${companionObj(qr)}"

  private def companionObj(qr: TypeTree.Object[Either[TypeRef, InputValue[TypeRef]]]): String = {
    val toVal = List(s"implicit val toInput: ToInputValue[${qr.meta.name}] = ToInputValue.derive")
    s"object ${qr.meta.name} ${curlyBlock(toVal)}"
  }

  private def enumObj(qr: TypeTree.Enum): String = {
    val values = qr.fields.map(f => s"case object $f extends ${qr.meta.name}")
    val contents = curlyBlock(values :+ "val values = findValues")
    s"object ${qr.meta.name} extends GraphQLEnum[${qr.meta.name}] $contents"
  }

  private def enum(qr: TypeTree.Enum): String =
    s"sealed trait ${qr.meta.name} extends EnumEntry\n\n${enumObj(qr)}"

  private def scalar(s: Scalar): String =
    s.meta.name match {
      case "String" | "Int" | "Boolean" | "Float" => ""
      case n => s"case class $n(value: String) extends AnyVal"
    }

  private def scalaCode(field: FlatType): String =
    field match {
      case o: TypeTree.Object[Either[TypeRef, InputValue[TypeRef]]] => caseClass(o)
      case e: TypeTree.Enum => enum(e)
      case s: TypeTree.Scalar => scalar(s)
      case _ => ""
    }

  private def obj(name: String, namespace: String, contents: String): String =
    s"""
       |package $namespace
       |import io.tvc.graphql.Runtime._
       |import io.tvc.graphql.input.ToInputObject
       |import io.circe.generic.JsonCodec
       |import enumeratum._
       |
       |object $name {
       |
       ${contents.lines.map(l => s"|$indent$l").mkString("\n")}
       |}
     """.stripMargin.trim


  private[generation] def queryLine1(g: Query): String = {
    val vars = g.inputs.fields.map(f => s"$$${f.name.value}:$${${f.`type`.value.name}.toInput.to(${f.name})}")
    s"${g.operation.toLowerCase} ${g.name}${vars.foldSmash("(", " ", ")")}"
  }

  private[generation] def queryFunction(q: Query): String = {
    val stringLines = queryLine1(q) :: q.selectionSets.lines.toList
    val lines = stringLines.map(l => s"|$l").foldSmash("\"\"\"\n", "\n", "\n\"\"\"")
    val widenedFields = q.inputs.fields.map(_.map[Either[TypeRef, InputValue[TypeRef]]](Right(_)))
    s"def ${q.name}${fields(widenedFields)}: String =\n${indent(lines)}.stripMargin"
  }

  def generate(generatable: Generateable): String =
    obj(
      generatable.query.name,
      generatable.namespace,
      (
        generatable.types.map(scalaCode).filter(_.nonEmpty).sorted :+
        queryFunction(generatable.query)
      ).mkString("\n\n")
    )
}
