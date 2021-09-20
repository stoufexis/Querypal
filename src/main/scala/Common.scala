import doobie.util.fragment.Fragment
import doobie.implicits._

object Common:

  case class Field[+B](name: Fragment)

  case class FieldValue(field: Field[Any], value: Fragment)

  case class PrimaryKey(field: Field[Any])
  case class ForeignKey(field: Field[Any], references: Model[?, ?])

  case class Table(private val nameStr: String):
    val name = sql"$nameStr"

  import cats.Monoid
  import cats.implicits._

  trait Fields

  trait ModelMeta[B]:
    val pk: PrimaryKey
    val fk: Option[ForeignKey] = None
    val table: Table
    def mapper(entity: B): List[FieldValue]

  case class Model[A, +B <: Fields](fields: B, meta: ModelMeta[A])

  trait Completable(val contents: List[Fragment]):
    def complete: Fragment = contents.fold(Monoid[Fragment].empty)(_ combine _)

end Common
