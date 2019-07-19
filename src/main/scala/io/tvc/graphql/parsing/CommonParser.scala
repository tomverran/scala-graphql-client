package io.tvc.graphql.parsing

import atto.Parser
import atto.parser.character.{char, letter, letterOrDigit, satisfy}
import atto.parser.combinator.{choice, many, manyN, opt}
import atto.parser.numeric.{float, int}
import atto.parser.text.{stringCI, stringOf, stringOf1}
import cats.Monad
import Combinators._
import io.tvc.graphql.parsing.CommonModel._
import atto.parser.character._
import atto.syntax.parser._
import io.tvc.graphql.parsing.CommonModel.Type._
import io.tvc.graphql.parsing.CommonModel.Value._
import cats.syntax.functor._
import cats.syntax.apply._

object CommonParser {

  val colon: Parser[Char] =
    ws(char(':'))

  val name: Parser[Name] =
    ws((stringOf1(snakeCase(letter)) ~ stringOf(snakeCase(letterOrDigit))).map(combine).map(Name.apply))

  /**
   * This section deals with string parsing
   * We're a little more lenient than the spec in terms of chars but eh
   */
  private val escapedUnicode: Parser[String] =
    (char('\\') ~ char('u') ~ manyN(4, letterOrDigit)).map { case ((a, b), c) => s"$a$b${c.mkString("")}" }

  private val escapedCharacter: Parser[String] =
    (char('\\') ~ oneOfChar('"', '\\', '/', 'b', 'f', 'n', 'r', 't')).map { case (a, b) => s"$a$b" }

  private val validStringChar: Parser[Char] =
    satisfy(c => c != '"' && c != '\\' && c != '\n')

  private val singleLineString: Parser[String] =
    char('"') ~> many(
      choice(validStringChar.map(_.toString), escapedCharacter, escapedUnicode)
    ).map(_.mkString("")) <~ char('"')

  private val blockString: Parser[String] =
    manyN(3, char('"')) ~> Monad[Parser].tailRecM("") { soFar =>
      (manyN(3, char('"')) || anyChar).map {
        case Right(char) => Left(soFar + char)
        case Left(_) => Right(soFar)
      }
    }

  val validString: Parser[String] =
    blockString | singleLineString

  /**
   * Parse the type (of a variable) - we don't support the ! operator
   * other than at the end of the type as it is difficult to recursively parse it
   * since it occurs after the recursive part of the parsing - actually maybe it does work
   */
   val`type`: Parser[Type] =
     (
       ws(
         choice[Type](
           name.map(Type.NamedType).widen,
           recSqBrackets(`type`).map(ListType).widen
         )
       ) ~ opt(ws(char('!')))
     ).map { case (t, bang) => bang.fold(t)(_ => NonNullType(t)) }

  val variable: Parser[Variable] =
    ws((char('$') ~> name).map(n => Variable(n.value)))

  val enumValue: Parser[EnumValue] =
    name.map(Value.EnumValue)

  lazy val objectValue: Parser[ObjectValue] =
    recCurlyBrackets(many((name <~ colon, value).mapN(ObjectField))).map(ObjectValue)

  lazy val listValue: Parser[ListValue] =
    recSqBrackets(many(value)).map(ListValue)

  val value: Parser[Value] =
    ws(
      choice(
        choice(str("true").as(BooleanValue(true)), str("false").as(BooleanValue(false))),
        validString.map(Value.StringValue).widen,
        variable.map(Value.VariableValue).widen,
        stringCI("null").as(Value.NullValue).widen,
        int.map(Value.IntValue).widen,
        float.map(Value.FloatValue).widen,
        objectValue.widen,
        enumValue.widen,
        listValue.widen
      )
    )

  val argument: Parser[Argument] =
    (name <~ ws(char(':')), value).mapN(Argument.apply)

  val argumentList: Parser[List[Argument]] =
    optRecParens(many(argument)).map(_.flatten)

  private val directive: Parser[Directive] =
    (ws(char('@')) ~> name, argumentList).mapN(Directive.apply)

  val directives: Parser[List[Directive]] =
    ws(opt(many(directive)).map(_.toList.flatten))
}
