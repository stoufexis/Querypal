import doobie.implicits._
import doobie.util.fragment.Fragment
import Common._
import cats.kernel.Monoid
import cats.implicits._
import doobie.syntax.SqlInterpolator.SingleFragment.fromFragment
import scala.annotation.targetName

object FragmentOperations:
  opaque type Command = Fragment

  opaque type Argument = Fragment

  opaque type ConditionOperator <: Argument = Fragment
  opaque type Condition <: Argument         = Fragment
  opaque type EqualsCondition <: Condition  = Fragment
  opaque type SetArgument <: Argument       = Fragment
  opaque type InsertArgument <: Argument    = Fragment

  /** Operators used in the query-building pipeline. They enable type checking
    * in the query construction and SQL-like syntax
    */
  trait FieldOps[A]:
    extension [B](x: Field[B, A])(using meta: ModelMeta[B])
      def ===(y: A): EqualsCondition = y match
        case z: Int =>
          sql"${meta.table.name}" ++ sql"." ++ fr"${x.name} = ${(z: Int)}"
        case z: String =>
          sql"${meta.table.name}" ++ sql"." ++ fr"${x.name} = ${(z: String)}"

      def set(y: A): SetArgument = y match
        case z: Int    => sql"${x.name} = ${z: Int}"
        case z: String => sql"${x.name} = ${z: String}"

  given FieldOps[Int] with
    extension [B](x: Field[B, Int])(using meta: ModelMeta[B])
      def >(y: Int): Condition =
        sql"${meta.table.name}" ++ sql"." ++ fr"${x.name} > $y"

      def <(y: Int): Condition =
        sql"${meta.table.name}" ++ sql"." ++ fr"${x.name} < $y"

  given FieldOps[String] with {
    extension [B](x: Field[B, String])(using meta: ModelMeta[B])
      def like(y: String): Condition =
        meta.table.name ++ fr".${x.name} like ${y}"
  }

  /** Helped methods that abstract the details of the sql syntax from the main
    * pipeline
    */
  object SqlOperations:
    def commaSeparatedParened(content: List[Fragment]): Argument =
      sql"(" |+| content
        .drop(1)
        .fold(content.head)((x, y) =>
          x ++ (GeneralOperators.comma ++ y)
        ) ++ GeneralOperators.rightParen

    def joinOp[A, B](using
        relation: Relation[A, B] | Relation[B, A],
        toMeta: ModelMeta[B]
    ): Argument =
      fr" inner join" ++ toMeta.table.name ++ fr" on" ++ relation.joinCondition

  object GeneralOperators:
    val leftParen: Argument  = fr"("
    val rightParen: Argument = fr")"
    val comma: Argument      = fr","

  object ConditionOperators:
    val and: ConditionOperator = fr"and"
    val or: ConditionOperator  = fr"or"

  object Commands:
    val update: Command = fr"update"
    val insert: Command = fr"insert into"
    val delete: Command = fr"delete from"
    val select: Command = fr"select * from"

  object Arguments:
    val where: Argument  = fr" where"
    val values: Argument = fr" values"
    val set: Argument    = fr" set"

  case class Table(name: Fragment)

  /** A trait that enables any part of the pipeline to become a terminal step
    */
  trait Completable(query: Query):
    def complete: Argument =
      query.arguments.foldFragments

    def construct: Fragment =
      (List(query.command, query.table.name) ++ query.arguments).foldFragments

    extension (content: List[Fragment])
      def foldFragments =
        content.fold(Monoid[Fragment].empty)(_ |+| _)

  val * : "*" = "*"