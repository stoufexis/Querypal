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
  opaque type SetArgument <: Argument       = Fragment
  opaque type InsertArgument <: Argument    = Fragment

  extension (content: List[Fragment])
    def foldFragments =
      content.fold(Monoid[Fragment].empty)(_ combine _)

  object FieldOps:

    extension (x: Field[Int]) def gt(y: Int): Condition = sql"${x.name} > $y "

    extension (x: Field[Int]) def lt(y: Int): Condition = sql"${x.name} < $y "

    extension [A](x: Field[A])
      def eqls(y: A): Condition = y match
        case z: Int    => sql"${x.name} = ${(z: Int)} "
        case z: String => sql"${x.name} = ${(z: String)} "

  object SqlOperations:
    def setArgument(fieldValues: List[FieldValue]): SetArgument = ((sql"set " +:
      fieldValues.flatMap { case FieldValue(field, value) =>
        List(field.name, sql" = ", value, sql", ")
      }).dropRight(1) :+ sql" ").foldFragments

    def commaSeparatedParened(content: List[Fragment]): Argument =
      sql"(" |+| content
        .drop(1)
        .fold(content.head)((x, y) => x combine (sql", " |+| y)) |+| sql") "

  object GeneralOperators:
    def leftParen: Argument  = sql"( "
    def rightParen: Argument = sql") "

  object ConditionOperators:
    val and: ConditionOperator = sql"and "
    val or: ConditionOperator  = sql"or "

  object Commands:
    val update: Command = sql"update "
    val insert: Command = sql"insert into "
    val delete: Command = sql"delete from "
    val select: Command = sql"select * from "

  object Arguments:
    val where: Argument  = sql"where "
    val values: Argument = sql"values "

  opaque type Table = Fragment

  object Table:
    def apply(nameStr: String): Table =
      println(nameStr)
      sql"$nameStr "

  trait Completable(query: Query):
    def complete: Argument =
      (List(query.command, query.table) ++ query.arguments).foldFragments

    def construct: Fragment =
      (List(query.command, query.table) ++ query.arguments).foldFragments
