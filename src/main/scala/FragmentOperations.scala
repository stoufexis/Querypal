import doobie.implicits._
import doobie.util.fragment.Fragment
import Common._
import cats.kernel.Monoid
import cats.implicits._

object FragmentOperations:

  opaque type Command = Fragment

  opaque type Argument = Fragment

  opaque type ConditionOperator <: Argument = Fragment
  opaque type Condition <: Argument         = Fragment
  opaque type EqualsCondition <: Condition  = Fragment
  opaque type SetArgument <: Argument       = Fragment
  opaque type InsertArgument <: Argument    = Fragment

  extension (content: List[Fragment])
    def foldFragments =
      content.fold(Monoid[Fragment].empty)(_ |+| _)

  sealed trait Field[A, B]:
    val name: Fragment

  case class Column[A, B](name: Fragment)     extends Field[A, B]
  case class PrimaryKey[A, B](name: Fragment) extends Field[A, B]
  case class ForeignKey[A, B](name: Fragment, references: Model[?, ?])
      extends Field[A, B]

  trait FieldOps[A] {
    extension [B](x: Field[A, B])
      def ===(y: A): EqualsCondition = y match
        case z: Int    => fr"${x.name} = ${(z: Int)}"
        case z: String => fr"${x.name} = ${(z: String)}"
  }

  given FieldOps[Int] with
    extension [B](x: Field[Int, B])
      def >(y: Int): Condition = fr"${x.name} > $y"
      def <(y: Int): Condition = fr"${x.name} < $y"

  given FieldOps[String] with {}

  object SqlOperations:

    val set: Argument = fr"set"

    def commaSeparatedParened(content: List[Fragment]): Argument =
      fr"(" |+| content
        .drop(1)
        .fold(content.head)((x, y) =>
          x ++ (GeneralOperators.comma ++ y)
        ) ++ GeneralOperators.rightParen

  // def setFieldValue(fv: FieldValue): Argument =
  //   fr"${fv.field.name} = ${fv.value}"

  object GeneralOperators:
    def leftParen: Argument  = fr"("
    def rightParen: Argument = fr")"
    def comma: Argument      = fr","

  object ConditionOperators:
    val and: ConditionOperator = fr"and"
    val or: ConditionOperator  = fr"or"

  object Commands:
    val update: Command = fr"update"
    val insert: Command = fr"insert into"
    val delete: Command = fr"delete from"
    val select: Command = fr"select * from"

  object Arguments:
    val where: Argument  = fr"where"
    val values: Argument = fr"values"

  opaque type Table = Fragment

  object Table:
    def apply(name: Fragment): Table = name

  trait Completable(query: Query):
    def complete: Argument =
      (List(query.command, query.table) ++ query.arguments).foldFragments

    def construct: Fragment =
      (List(query.command, query.table) ++ query.arguments).foldFragments
