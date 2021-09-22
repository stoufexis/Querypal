import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

object Common:

  type FieldValue = (Field[?, ?], Fragment)

  object ModelGen:
    trait ModelGen[A]:
      def apply[B](fields: B): Model[A, B] = Model(fields)

    given [A]: ModelGen[A] with {}

    def apply[A](using mg: ModelGen[A]) = mg

  case class Model[A, B](fields: B)

  trait ModelMeta[A](val tableName: Fragment):
    val table: Table = Table(tableName)
    def mapper(entity: A): List[FieldValue]

  case class Query(command: Command, table: Table, arguments: List[Argument])
