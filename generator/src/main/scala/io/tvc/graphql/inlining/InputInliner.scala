package io.tvc.graphql.inlining

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Eval, Traverse}
import io.tvc.graphql.parsing.QueryModel.{OperationDefinition, VariableDefinition}
import io.tvc.graphql.parsing.SchemaModel.TypeDefinition.InputObjectTypeDefinition
import io.tvc.graphql.parsing.SchemaModel.{InputValueDefinition, Schema, TypeDefinition}
import io.tvc.graphql.recursion.Fix
import io.tvc.graphql.recursion.Fix.unfoldF
import io.tvc.graphql.inlining.Utilities._
import io.tvc.graphql.inlining.Utilities.TypeError.OrMissing
import io.tvc.graphql.inlining.TypeTree.{Field, FieldName, Metadata, Object}

import scala.language.higherKinds

/**
  * Here we take a query and a schema and produce a list InputTypeTree ASTs.
  * This is a recursive structure of complex types required by the query
  */
object InputInliner {

  /**
    * An InputValue is the same structurally as an output value
    * but it also can have a default value so we need to wrap it
    */
  case class InputValue[+A](default: Option[String], value: A)

  object InputValue {

    implicit val traverse: Traverse[InputValue] =
      new Traverse[InputValue] {
        def traverse[G[_]: Applicative, A, B](fa: InputValue[A])(f: A => G[B]): G[InputValue[B]] =
          f(fa.value).map(b => fa.copy(value = b))
        def foldLeft[A, B](fa: InputValue[A], b: B)(f: (B, A) => B): B =
          f(b, fa.value)
        def foldRight[A, B](fa: InputValue[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          f(fa.value, lb)
      }
  }

  type InputObject[A] = TypeTree.Object[InputValue[A]]
  type InputField = Field[InputValue[TypeDefinition]]
  type InputTypeTree[+A] = TypeTree[InputValue[A]]
  type RecInputTypeTree = Fix[InputTypeTree]

  implicit val traverse: Traverse[InputTypeTree] =
    TypeTree.ttTraverse.compose(InputValue.traverse)


  /**
    * Given an InputValueDefinition create an input field which
    * contains a raw TypeDefinition that will be expanded by unfoldF
    */
  def createField(schema: Schema)(v: InputValueDefinition): OrMissing[InputField] =
    findTypeDefinition(schema, v.`type`).map { defn =>
      Field(
        name = FieldName(alias = None, value = v.name.value),
        `type` = InputValue(v.defaultValue.map(_.toString), defn),
        modifiers = modifiers(v.`type`).toList
      )
    }

  /**
    * Create metadata for an input object,
    * preserving the description + name
    */
  def metadata(i: InputObjectTypeDefinition): Metadata =
    Metadata(i.description.map(_.value), i.name.value)

  /**
    * Try to create a complex type from this type definition,
    * will fail if the type isn't an input object
    */
  def complex(schema: Schema, td: TypeDefinition): OrMissing[InputTypeTree[TypeDefinition]] =
    td match {
      case i: InputObjectTypeDefinition => i.values.traverse(createField(schema)).map(Object(metadata(i), _))
      case e => Left(TypeError.Todo(s"Expected input object, got $e"))
    }

  /**
    * Try to create a simple type from this type definition,
    * will fail if the type isn't a scalar (Enum / etc)
    */
  def simple(td: TypeDefinition): OrMissing[InputTypeTree[TypeDefinition]] =
    OutputInliner.createScalar(td)

  /**
    * Run the inlining process on the given query, taking all of it's arguments
    * and thus producing a recursive list of input types the user must submit to the server
    */
  def run(schema: Schema, query: List[VariableDefinition]): OrMissing[InputObject[RecInputTypeTree]] =
    query.traverse { v =>
      findTypeDefinition(schema, v.`type`).flatMap { td =>
        unfoldF(td)(t => simple(t).orElse(complex(schema, t))).map { tree =>
          Field(
            FieldName(None, v.variable.value),
            InputValue(default = v.default.map(_.toString), value = tree),
            Utilities.modifiers(v.`type`).toList
          )
        }
      }
    }.map { fields =>
      Object(meta = Metadata(comment = None, name = "Inputs"), fields = fields)
    }
}
