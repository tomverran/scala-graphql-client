package io.tvc.graphql.transform

import cats.Monad
import cats.data.State
import io.tvc.graphql.transform.TypeTree.{Enum, Metadata, Object, RecTypeTree, Scalar, Union}

object TypeDeduplicator {

  case class TypeRef(name: String)
  type FlatType = TypeTree[TypeRef]
  type TypeStore = Map[TypeRef, FlatType]
  type TypeState[A] = State[TypeStore, A]

  def name(t: FlatType): TypeRef =
    TypeRef(t.meta.name)

  def rename(typeTree: FlatType, f: String => String): FlatType =
    typeTree match {
      case Scalar(m) => Scalar(Metadata(m.comment, f(m.name)))
      case Enum(m, vs) => Enum(Metadata(m.comment, f(m.name)), vs)
      case Union(m, vs) => Union(Metadata(m.comment, f(m.name)), vs)
      case Object(m, vs) => Object(Metadata(m.comment, f(m.name)), vs)
    }

  /**
    * Given a type to make globally available
    * lift it into the global type store and return its new name
    */
  def register(tpe: TypeTree[TypeRef]): TypeState[TypeRef] =
    Monad[TypeState].tailRecM(tpe) { t =>
      val ref = name(t)
      State { existing =>
        existing.get(ref).fold[(TypeStore, Either[FlatType, TypeRef])](
          existing.updated(ref, t) -> Right(ref)
        ) { clash =>
          existing -> Either.cond(clash == t, ref, rename(t, _ + "2"))
        }
      }
    }

  /**
    * Turn a recursive tree of types into a flat list of types
    * with any duplicate types being renamed to avoid clashes
    */
  def run(tree: RecTypeTree): List[FlatType] =
    Fix.foldF[TypeTree, TypeState, TypeRef](tree)(register).runS(Map.empty).value.values.toList
}
