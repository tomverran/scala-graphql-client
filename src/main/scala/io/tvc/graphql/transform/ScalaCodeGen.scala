package io.tvc.graphql.transform
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.foldable._
import io.tvc.graphql.transform.TypeDeduplicator.{FlatType, TypeRef}
import io.tvc.graphql.transform.TypeTree.TypeModifier.{ListType, NonNullType, NullableType}
import io.tvc.graphql.transform.TypeTree.{Scalar, TypeModifier}

object ScalaCodeGen {

  private val indent: String = " "

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
    s"${qr.meta.comment.foldMap(blockComment)}case class ${qr.meta.name}${fields(qr.fields)}"

  private def sealedTraitObj(qr: TypeTree.Enum): String =
    s"object ${qr.meta.name} ${curlyBlock(qr.fields.map(f => s"case object $f extends ${qr.meta.name}"))}"

  private def sealedTrait(qr: TypeTree.Enum): String =
    s"sealed trait ${qr.meta.name}\n${sealedTraitObj(qr)}"

  private def scalar(s: Scalar): String =
    s.meta.name match {
      case "String" | "Int" | "Boolean" | "Float" => ""
      case n => s"case class $n(value: String) extends AnyVal"
    }

  private def scalaCode(field: FlatType): String =
    field match {
      case o: TypeTree.Object[TypeRef] => caseClass(o)
      case e: TypeTree.Enum => sealedTrait(e)
      case s: TypeTree.Scalar => scalar(s)
      case _ => ""
    }

  def generate(input: List[FlatType]): String =
    input.map(scalaCode).filter(_.nonEmpty).sorted.mkString("\n\n")
}
