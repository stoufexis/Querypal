import doobie.implicits._
import doobie.util.fragment.Fragment
import Common._
import cats.kernel.Monoid
import cats.implicits._
import FragmentOperations.PrimaryKey
import javax.management.relation.Relation

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

  sealed trait Field[+A, B]:
    val name: Fragment
  case class Column[A, B](name: Fragment)     extends Field[A, B]
  case class PrimaryKey[A, B](name: Fragment) extends Field[A, B]

  trait Relationship[A, B]:
    val fieldToField: (Field[?, A], Field[?, B])

  trait OneToMany[A, B](fields: (Field[?, A], Field[?, B]))
      extends Relationship[A, B]:
    val fieldToField = fields

  trait ManyToOne[A, B](fields: (Field[?, A], Field[?, B]))
      extends Relationship[A, B]:
    val fieldToField = fields

  trait OneToOne[A, B](fields: (Field[?, A], Field[?, B]))
      extends Relationship[A, B]:
    val fieldToField = fields

  trait FieldOps[A]:
    extension [B](x: Field[A, B])
      def ===(y: A): EqualsCondition = y match
        case z: Int    => fr"${x.name} = ${(z: Int)}"
        case z: String => fr"${x.name} = ${(z: String)}"

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

    //select * from person inner join photo on person.name = photo.photographer_name where person.name = 'Stef'
    def joinOp[A, B](using
        meta: ModelMeta[A],
        toJoinMeta: ModelMeta[B],
        relation: Relationship[A, B] | Relationship[B, A]
    ): Argument =
      fr"inner join"
        ++ toJoinMeta.tableName
        ++ fr" on"
        ++ meta.tableName
        ++ sql"."
        ++ relation.fieldToField._1.name
        ++ fr"="
        ++ toJoinMeta.tableName
        ++ sql"."
        ++ relation.fieldToField._2.name

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
    def apply(name: Fragment): Table = name ++ sql" "

  trait Completable(query: Query):
    def complete: Argument =
      (List(query.command, query.table) ++ query.arguments).foldFragments

    def construct: Fragment =
      (List(query.command, query.table) ++ query.arguments).foldFragments
