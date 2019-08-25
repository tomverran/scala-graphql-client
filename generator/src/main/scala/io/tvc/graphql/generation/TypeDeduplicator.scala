package io.tvc.graphql.generation

import cats.data.State
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Monad, Traverse, ~>}
import io.tvc.graphql.inlining.InputInliner.{InputObject, InputTypeTree, InputValue, RecInputTypeTree}
import io.tvc.graphql.inlining.TypeTree
import io.tvc.graphql.inlining.TypeTree.{Enum, Metadata, Object, RecTypeTree, Scalar, Union}
import io.tvc.graphql.recursion.Fix

object TypeDeduplicator {

  case class Output(
    types: List[FlatType],
    variables: Object[InputValue[TypeRef]]
  )

  case class TypeRef(name: String)

  type FlatType = TypeTree[Either[TypeRef, InputValue[TypeRef]]]
  type FlatInputType = TypeTree[InputValue[TypeRef]]

  type TypeStore = Map[TypeRef, FlatType]
  type TypeState[A] = State[TypeStore, A]

  def name(t: FlatType): TypeRef =
    TypeRef(t.meta.name)

  implicit val oTraverse: Traverse[InputObject] =
    TypeTree.objTraverse.compose(InputValue.traverse)

  def rename(f: String => String): TypeTree ~> TypeTree = new (TypeTree ~> TypeTree) {
    def apply[A](fa: TypeTree[A]): TypeTree[A] =
      fa match {
        case Scalar(m) => Scalar(Metadata(m.comment, f(m.name)))
        case Enum(m, vs) => Enum(Metadata(m.comment, f(m.name)), vs)
        case Union(m, vs) => Union(Metadata(m.comment, f(m.name)), vs)
        case Object(m, vs) => Object(Metadata(m.comment, f(m.name)), vs)
      }
  }

  /**
    * Given a type to make globally available
    * lift it into the global type store and return its new name
    */
  def register(tpe: FlatType): TypeState[TypeRef] =
    Monad[TypeState].tailRecM(tpe) { t =>
      val ref = name(t)
      State { existing =>
        existing.get(ref).fold[(TypeStore, Either[FlatType, TypeRef])](
          existing.updated(ref, t) -> Right(ref)
        ) { clash =>
          existing -> Either.cond(clash == t, ref, rename(_ + "2")(t))
        }
      }
    }

  def toFlat(i: InputTypeTree[TypeRef]): FlatType =
    (i: TypeTree[InputValue[TypeRef]]).map(Right(_))

  /**
    * Turn a recursive tree of types into a flat list of types
    * with any duplicate types being renamed to avoid clashes
    */
  def deduplicate(variables: InputObject[RecInputTypeTree], output: RecTypeTree): Output =
    (
      for {
        _ <- Fix.foldF[TypeTree, TypeState, TypeRef](output)(l => register(l.map(Left(_))))
        input <- variables.traverse(Fix.foldF[InputTypeTree, TypeState, TypeRef](_)(l => register(toFlat(l))))
        values <- State.get
      } yield Output(values.values.toList, input)
    ).runA(Map.empty).value
}
