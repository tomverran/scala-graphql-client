package io.tvc.graphql.parsing

object CommonModel {

  /**
   *  https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
   */
  sealed trait OperationType

  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
    val values = List(Query, Mutation, Subscription)
  }

  /**
   * https://graphql.github.io/graphql-spec/June2018/#Name
   */
  case class Name(value: String) extends AnyVal

  /**
   * https://graphql.github.io/graphql-spec/June2018/#Argument
   */
  case class Argument(name: Name, value: Value)

  /**
   * https://graphql.github.io/graphql-spec/June2018/#ObjectField
   */
  case class ObjectField(name: Name, value: Value)

  /**
   * https://graphql.github.io/graphql-spec/June2018/#Variable
   */
  case class Variable(value: String) extends AnyVal

  /**
   * https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
   */
  case class Directive(name: Name, arguments: List[Argument])

  /**
   * https://graphql.github.io/graphql-spec/June2018/#sec-Input-Values
   */
  sealed trait Value

  object Value {
    case object NullValue extends Value
    case class IntValue(value: Int) extends Value
    case class EnumValue(value: Name) extends Value
    case class FloatValue(value: Float) extends Value
    case class StringValue(value: String) extends Value
    case class BooleanValue(value: Boolean) extends Value
    case class ListValue(value: List[Value]) extends Value
    case class VariableValue(value: Variable) extends Value
    case class ObjectValue(value: List[ObjectField]) extends Value
  }

  /**
   * https://graphql.github.io/graphql-spec/June2018/#sec-Type-References
   */
  sealed trait Type

  object Type {
    case class NonNullType(value: Type) extends Type
    case class NamedType(value: Name) extends Type
    case class ListType(value: Type) extends Type
  }
}
