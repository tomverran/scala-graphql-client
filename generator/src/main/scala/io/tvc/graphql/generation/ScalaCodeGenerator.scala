package io.tvc.graphql.generation

import TypeDeduplicator.{FlatType, TypeRef}
import io.tvc.graphql.inlining.TypeTree
import io.tvc.graphql.inlining.TypeTree.TypeModifier.{ListType, NonNullType, NullableType}
import io.tvc.graphql.inlining.TypeTree.{Scalar, TypeModifier}
import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._
import cats.instances.string._

object ScalaCodeGenerator {

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

  private def fields(fs: List[TypeTree.Field[TypeRef]]): String =
    argList(fs.map(f => s"${f.name.alias.getOrElse(f.name.value)}: ${fieldType(f.`type`, f.modifiers)}"))

  private def caseClass(qr: TypeTree.Object[TypeRef]): String =
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
      case "String" | "Int" | "Boolean" | "Float" => ""
      case n => s"case class $n(value: String) extends AnyVal"
    }

  private def scalaCode(field: FlatType): String =
    field match {
      case o: TypeTree.Object[TypeRef] => caseClass(o)
      case e: TypeTree.Enum => enum(e)
      case s: TypeTree.Scalar => scalar(s)
      case _ => ""
    }

  private def obj(name: String, namespace: String, contents: String): String =
    s"""
       |package $namespace
       |import io.tvc.graphql.Runtime._
       |import io.circe.generic.JsonCodec
       |import enumeratum._
       |
       |object $name {
       |
       ${contents.lines.map(l => s"|$indent$l").mkString("\n")}
       |}
     """.stripMargin.trim

  private def queryVal(query: String): String = {
    val lines = query.lines.map(l => s"|$l").toList.foldSmash("\"\"\"\n", "\n", "\n\"\"\"")
    s"val query: String = \n${indent(lines)}.stripMargin"
  }

  def generate(name: String, namespace: String, query: String, input: List[FlatType]): String =
    obj(name, namespace, (input.map(scalaCode).filter(_.nonEmpty).sorted :+ queryVal(query)).mkString("\n\n"))
}
