import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

object Common:

  type FieldValue[A] = (Field[?, A], Fragment)

  trait Model[A]

  // case class Model[A, B](fields: B)

  trait ModelMeta[A](tableName: Fragment):
    val pk: PrimaryKey[Any, A]
    val table = Table(tableName)
    def mapper(entity: A): List[FieldValue[A]]

  case class Query(command: Command, table: Table, arguments: List[Argument])
