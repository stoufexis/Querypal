import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

object Common:

  type FieldValue = (Field[?, ?], Fragment)

  import cats.Monoid
  import cats.implicits._

  trait Fields[A]

  case class Model[A, B <: Fields[A]](fields: B)

  trait ModelMeta[B]:
    val table: Table
    def mapper(entity: B): List[FieldValue]

  case class Query(command: Command, table: Table, arguments: List[Argument])
