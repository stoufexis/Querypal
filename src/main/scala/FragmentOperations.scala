import doobie.implicits._
import Common._
import cats.kernel.Monoid
import cats.implicits._
import doobie.syntax.SqlInterpolator.SingleFragment.fromFragment
import scala.annotation.targetName
import doobie.Update0
import doobie.util.fragment.Fragment

object FragmentOperations:
  opaque type Command = String

  opaque type Argument = String

  opaque type ConditionOperator <: Argument = String
  opaque type Condition <: Argument         = String
  opaque type EqualsCondition <: Condition  = String
  opaque type SetArgument <: Argument       = String
  opaque type InsertArgument <: Argument    = String

  /** Operators used in the query-building pipeline. They enable type checking
    * in the query construction and SQL-like syntax
    */
  trait FieldOps[A]:
    extension [B](x: Field[B, A])(using meta: ModelMeta[B])
      def ===(y: A): EqualsCondition = y match
        case z: Int =>
          s"${meta.table.name}" ++ s"." ++ s"${x.name} = ${(z: Int)} "
        case z: String =>
          s"${meta.table.name}" ++ s"." ++ s"${x.name} = '${(z: String)}' "

      def set(y: A): SetArgument = y match
        case z: Int    => s"${x.name} = ${z: Int} "
        case z: String => s"${x.name} = '${z: String}' "

  given FieldOps[Int] with
    extension [B](x: Field[B, Int])(using meta: ModelMeta[B])
      def >(y: Int): Condition =
        s"${meta.table.name}" ++ s"." ++ s"${x.name} > $y "

      def <(y: Int): Condition =
        s"${meta.table.name}" ++ s"." ++ s"${x.name} < $y "

  given FieldOps[String] with {
    extension [B](x: Field[B, String])(using meta: ModelMeta[B])
      def like(y: String): Condition =
        meta.table.name ++ s".${x.name} like '${y}' "
  }

  /** Helped methods that abstract the details of the sql syntax from the main
    * pipeline
    */

  object GeneralOperators:
    val leftParen: Argument  = s"( "
    val rightParen: Argument = s") "
    val comma: Argument      = s", "

  object ConditionOperators:
    val and: ConditionOperator = s"and "
    val or: ConditionOperator  = s"or "

  object Commands:
    val update: Command = s"update "
    val insert: Command = s"insert into "
    val delete: Command = s"delete from "
    val select: Command = s"select * from "

  object Arguments:
    val where: Argument  = s" where "
    val values: Argument = s" values "
    val set: Argument    = s" set "

  case class Table(name: String)

  /** A trait that enables any part of the pipeline to become a terminal step
    */

  object SqlOperations:
    def commaSeparatedParened(content: List[String]): Argument =
      s"(" |+| content
        .drop(1)
        .fold(content.head)((x, y) =>
          x ++ (GeneralOperators.comma ++ y)
        ) ++ GeneralOperators.rightParen

    def joinOp[A, B](using
        relation: Relation[A, B] | Relation[B, A],
        toMeta: ModelMeta[B]
    ): Argument =
      s"inner join " + toMeta.table.name + s"on " + relation.joinCondition

    def complete(query: Query): Argument =
      query.arguments.foldStrings

    def construct(query: Query): Fragment =
      Update0(
        (List(query.command, query.table.name) ++ query.arguments).foldStrings,
        None
      ).toFragment

    extension (content: List[String])
      private def foldStrings =
        content.fold(Monoid[String].empty)(_ |+| _)

  trait Completable:
    def complete: Argument

    def construct: Fragment

  val * : "* " = "* "
