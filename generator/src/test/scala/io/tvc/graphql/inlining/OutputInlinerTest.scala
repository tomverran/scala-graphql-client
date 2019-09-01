package io.tvc.graphql.inlining

import cats.data.NonEmptyList
import io.tvc.graphql.inlining.Utilities.TypeError.MissingType
import io.tvc.graphql.parsing.CommonModel.{Name, OperationType}
import io.tvc.graphql.parsing.QueryModel.SelectionSet
import io.tvc.graphql.parsing.SchemaModel.{RootOperationTypeDefinition, Schema, SchemaDefinition}
import org.scalacheck.{Gen, Prop}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

class OutputInlinerTest extends WordSpec with Matchers with Checkers {

  "Output inliner" should {

    "Return an error if there is no schema definition & no type with the name of the op type" in {
      check(
        Prop.forAll(Gen.oneOf(OperationType.values)) { op =>
          val schema = Schema(schemaDefinition = None, typeDefinitions = Map.empty)
          OutputInliner.run(schema, op, SelectionSet(List.empty)) == Left(MissingType(op.toString))
        }
      )
    }

    "Return an error if there is a schema definition and the type corresponding to the op doesn't exist" in {
      check(
        Prop.forAll(Gen.oneOf(OperationType.values), Gen.alphaNumStr) { case (op, name) =>
          val defn = SchemaDefinition(NonEmptyList.of(RootOperationTypeDefinition(op, Name(name))))
          val schema = Schema(schemaDefinition = Some(defn), typeDefinitions = Map.empty)
          OutputInliner.run(schema, op, SelectionSet(List.empty)) == Left(MissingType(name))
        }
      )
    }
  }
}
