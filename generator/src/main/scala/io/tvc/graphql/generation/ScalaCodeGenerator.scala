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
    stringValue: String,
    inputs: Option[InputObject[TypeRef]]
  )

  case class GenerationInput(
    namespace: String,
    rootName: TypeRef,
    types: List[FlatType],
    query: Query
  )

  private val indent: String = "  "

  private def indent(s: String): String =
    s.lines.map(l => if (l.isEmpty) "" else s"$indent$l").mkString("\n")

  private def blockComment(s: String): String =
    s.lines.toList.filter(_.nonEmpty).map(l => s"$indent* $l").foldSmash("/**\n", "\n", s"\n$indent*/\n")

  private def argList(as: List[String], forceVertical: Boolean = false): String =
    if (as.isEmpty) {
      ""
    } else if (as.length >= 3 || forceVertical) {
      as.foldSmash(s"(\n$indent", s",\n$indent", "\n)")
    } else {
      as.foldSmash("(", ", ", ")")
    }

  private def curlyBlock(what: List[String]): String =
    what.foldSmash(s"{\n$indent", s"\n$indent", "\n}")

  private def scalaType(tpe: TypeRef, mods: List[TypeModifier]): String =
    mods.foldLeft(tpe.name) {
      case (n, NonNullType) => n
      case (n, ListType) => s"List[$n]"
      case (n, NullableType) => s"Option[$n]"
    }

  private def fields(fs: List[TypeTree.Field[Either[TypeRef, InputValue[TypeRef]]]]): String =
    argList(fs.map(f => s"${f.name.alias.getOrElse(f.name.value)}: ${scalaType(f.`type`.map(_.value).merge, f.modifiers)}"))

  private[generation] def caseClass(qr: TypeTree.Object[Either[TypeRef, InputValue[TypeRef]]]): String =
    s"${qr.meta.comment.foldMap(blockComment)}@JsonCodec\ncase class ${qr.meta.name}${fields(qr.fields)}"

  private def enumObj(qr: TypeTree.Enum): String = {
    val values = qr.fields.map(f => s"case object $f extends ${qr.meta.name}")
    val contents = curlyBlock(values :+ "val values = findValues")
    s"object ${qr.meta.name} extends GraphQLEnum[${qr.meta.name}] $contents"
  }

  private def enum(qr: TypeTree.Enum): String =
    s"sealed trait ${qr.meta.name} extends EnumEntry\n\n${enumObj(qr)}"

  private def scalar(s: Scalar): String =
    s.meta.name match {
      case "String" | "Int" | "Boolean" | "Float" | "ID" => ""
      case n => s"case class $n(value: String) extends AnyVal"
    }

  private[generation] def scalaCode(field: FlatType): String =
    field match {
      case o: TypeTree.Object[Either[TypeRef, InputValue[TypeRef]]] => caseClass(o)
      case e: TypeTree.Enum => enum(e)
      case s: TypeTree.Scalar => scalar(s)
      case _ => ""
    }

  private def obj(name: String, namespace: String, contents: String): String =
    s"""
       |package $namespace
       |import io.tvc.graphql.Builtin._
       |import io.tvc.graphql.Request
       |import io.tvc.graphql.Response
       |import io.circe.generic.JsonCodec
       |import io.circe.syntax._
       |import enumeratum._
       |
       |object $name {
       |
       ${contents.lines.map(l => s"|$indent$l").mkString("\n")}
       |}
     """.stripMargin.trim

  private def queryArguments(q: Query): String =
    q.inputs.fold("") { f =>
      argList(f.fields.map(f => s"${f.name.value}: ${scalaType(f.`type`.value, f.modifiers)}"))
    }

  private def queryVariables(q: InputObject[TypeRef]): String =
    s"${q.meta.name}${argList(q.fields.map(_.name.value))}.asJson"

  private def queryBlockString(q: Query): String =
    q.stringValue.lines.map(l => s"|$l").toList.foldSmash(
      prefix = "\"\"\"\n",
      delim = "\n",
      suffix = "\n\"\"\".trim.stripMargin"
    )

  private[generation] def queryFunction(q: Query, rootName: TypeRef): String = {
    val variables = q.inputs.toList.map(q => s"variables = ${queryVariables(q)}")
    val args = List(s"query = ${indent(queryBlockString(q)).trim}") ++ variables
    val request = s"Request[Response[${rootName.name}]]${argList(args, forceVertical = true)}"
    s"def apply${queryArguments(q)}: Request[Response[${rootName.name}]] =\n${indent(request)}"
  }

  private def toFlat(l: InputObject[TypeRef]): FlatType =
    (l: TypeTree.Object[InputValue[TypeRef]]).map(Right(_))

  def generate(input: GenerationInput): String =
    obj(
      input.query.name,
      input.namespace,
      (
        (
          input.query.inputs.map(toFlat).toList ++ input.types
        ).map(scalaCode).filter(_.nonEmpty).sorted :+
        queryFunction(input.query, input.rootName)
      ).mkString("\n\n")
    )
}
