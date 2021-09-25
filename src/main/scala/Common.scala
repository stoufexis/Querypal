import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import scala.deriving.Mirror
import Common.ModelMeta
import scala.compiletime.summonInline

object Common:

  type FieldValue[A] = (Field[?, A], Fragment)

  trait Model[A]:
    protected def makeColumn[B](name: Fragment): Column[B, A] = Column(name)

  trait ModelMeta[A: Mirror.ProductOf: ModelMeta]:
    // def makeColumn[B](name: Fragment): Column[A, B]
    val primaryKey: PrimaryKey[Any, A]
    val table: Table
    def map(a: A): Tuple2[Iterator[Fragment], Iterator[Fragment]]

  case class Query(command: Command, table: Table, arguments: List[Argument])

  case class GenericCol[A](name: Fragment)
