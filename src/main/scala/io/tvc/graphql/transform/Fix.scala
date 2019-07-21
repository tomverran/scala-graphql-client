package io.tvc.graphql.transform

import scala.language.higherKinds


case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {


}
