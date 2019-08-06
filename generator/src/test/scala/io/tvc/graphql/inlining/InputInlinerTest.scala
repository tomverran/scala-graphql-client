package io.tvc.graphql.inlining

import io.tvc.graphql.parsing.{Loader, QueryParser, SchemaParser}
import org.scalatest.{Matchers, WordSpec}

class InputInlinerTest extends WordSpec with Matchers {

  "Output type checker" should {

    "Work" in {


      val q = QueryParser.parse(Loader.load("/queries/query.graphql")).right.get
      val s = SchemaParser.parse(Loader.load("/schemas/github.idl")).right.get


      println(InputInliner.run(s, q))
      pending

    }


  }


}
