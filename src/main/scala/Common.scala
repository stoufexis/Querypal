import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

object Common:

  case class Field[+B](name: Fragment)

  case class FieldValue(field: Field[Any], value: Fragment)

  case class PrimaryKey(field: Field[Any])
  case class ForeignKey(field: Field[Any], references: Model[?, ?])

  import cats.Monoid
  import cats.implicits._

  trait Fields

  trait ModelMeta[B]:
    val pk: PrimaryKey
    val fk: Option[ForeignKey] = None
    val table: Fragment
    def mapper(entity: B): List[FieldValue]

  case class Model[A, +B <: Fields](fields: B, meta: ModelMeta[A])

  case class Query(command: Command, table: Fragment, arguments: List[Argument])
