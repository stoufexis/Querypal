import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

object Common:

  case class FieldValue(field: Field[Any], value: Fragment)

  import cats.Monoid
  import cats.implicits._

  trait Fields {}

  trait ModelMeta[B]:
    val table: Fragment
    def mapper(entity: B): List[FieldValue]

  case class Model[A, +B <: Fields](fields: B, meta: ModelMeta[A])

  case class Query(command: Command, table: Fragment, arguments: List[Argument])
