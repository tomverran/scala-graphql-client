package io.tvc.graphql

import cats.instances.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Id, Monad, Traverse}

import scala.language.higherKinds

/**
  * The fix of a recursive data type
  * to make it representable in Scala
  */
case class Fix[F[+_]](unfix: F[Fix[F]])

object Fix {

  /**
    * Given a seed value perform an effect to construct a tree
    * The opposite of the folds below but just as thrillingly un stack safe
    */
  def unfoldF[F[+_]: Traverse, G[_]: Monad, A](a: A)(f: A => G[F[A]]): G[Fix[F]] =
    f(a).flatMap(_.traverse(unfoldF(_)(f)).map(Fix(_)))

  /**
    * Find out if we're at the end of a tree by using the traverse instance of the fixed type
    * If we're at the end of the tree we'll return the leaf type (i.e. an F[Nothing])
    */
  def partition[F[+_]: Traverse](l: Fix[F]): Either[F[Fix[F]], F[Nothing]] =
    l.unfix.traverse[Either[F[Fix[F]], ?], Nothing](_ => Left(l.unfix))

  /**
    * Non stack safe fold,
    * but it does the job for now
    */
  def fold[F[+_]: Traverse, A](in: Fix[F])(f: F[A] => A): A =
    foldF[F, Id, A](in)(f)

  /**
    * Same as above but works with functions returning results lifted into a Monad, G
    * Also brutally non stack safe (I think)
    */
  def foldF[F[+_]: Traverse, G[_]: Monad, A](in: Fix[F])(f: F[A] => G[A]): G[A] =
    partition(in) match {
      case Right(v) => f(v)
      case Left(v) => v.traverse(foldF(_)(f)).flatMap(f)
    }
}
