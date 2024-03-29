package io.tvc.graphql.parsing

import atto.Parser
import atto.Parser.{Failure, Success, TResult}
import atto.parser.character.{char, whitespace, notChar}
import atto.parser.combinator.{choice, discardLeft, many, opt}
import atto.parser.text.stringCI
import atto.parser.{character, combinator}
import atto.syntax.parser._

object Combinators {

  def str(value: String): Parser[String] =
    ws(stringCI(value))

  /**
   * Atto stack overflows with recursive parser combinators often
   * because most of the toString methods evalulate any parsers being combined.
   * This breaks that chain by not evaluating the underlying parser
   */
  def rec[A](p: => Parser[A]): Parser[A] =
    new Parser[A] {
      def apply[R](st0: Parser.State, kf: Failure[R], ks: Success[A, R]): TResult[R] = p(st0, kf, ks)
      override def toString = "rec(...)"
    }

  /**
   * version of the square brackets combinator
   * that works with recursive parsers
   */
  def recSqBrackets[A](p: => Parser[A]): Parser[A] =
    ws(rec(discardLeft(ws(char('[')), p) <~ ws(char(']'))))

  def optRecParens[A](p: => Parser[A]): Parser[List[A]] =
    ws(opt(rec(discardLeft(ws(char('(')), p) <~ ws(char(')')))).map(_.toList))

  def recCurlyBrackets[A](p: => Parser[A]): Parser[A] =
    ws(rec(discardLeft(ws(char('{')), p) <~ ws(char('}'))))

  private val ignored: Parser[Char] =
    whitespace | char(',')

  private val comment: Parser[String] =
    many(ignored) ~> char('#') ~> many(notChar('\n')).map(_.mkString) <~ char('\n') <~ many(ignored)

  /**
   * Given a parser,
    * discard any trailing whitespace or leading / trailing comments
   */
  def ws[A](p: => Parser[A]): Parser[A] =
    opt(comment) ~> p <~ many(ignored) <~ opt(comment)

  def oneOfChar(chars: Char*): Parser[Char] =
    choice(chars.map(char):_*)

  def snakeCase(p: Parser[Char]): Parser[Char] =
    combinator.choice(p, character.char('_'))

  def combine(a: (String, String)): String =
    a._1 + a._2

  def commaSeparated[A](p: Parser[A]): Parser[List[A]] =
    separatedWith(p, ',')

  def separatedWith[A](p: Parser[A], sep: Char): Parser[List[A]] =
    opt(ws(char(sep))) ~> (many(p <~ ws(char(sep))) ~ opt(p)).map { case (h, t) => h ++ t.toList }

}
