package io.tvc.graphql.parsing

import scala.io.Source

object Loader {

  def load(file: String): String = {
    val source = Source.fromURL(getClass.getResource(file))
    val lines = source.getLines.mkString("\n")
    source.close
    lines
  }
}
