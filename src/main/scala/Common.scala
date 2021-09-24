import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

object Common:

  type FieldValue = (Field[?], Fragment)

  trait Model[A]:
    val pk = PrimaryKey[String](fr"name")

  // case class Model[A, B](fields: B)

  trait ModelMeta[A](tableName: Fragment):
    val table = Table(tableName)
    def mapper(entity: A): List[FieldValue]

  case class Query(command: Command, table: Table, arguments: List[Argument])
