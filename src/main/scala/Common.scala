import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import scala.deriving.Mirror
import Common.ModelMeta
import scala.compiletime.summonInline

object Common:

  type FieldValue[A] = (Field[?, A], Fragment)

  trait Model[A]:
    protected def column[B](name: Fragment): Column[B, A] = Column(name)
    protected def foreignKey[B, C](field: Field[B, C])(
        name: Fragment
    ): ForeignKey[B, A, C] = ForeignKey(name)

  trait ModelMeta[A: Mirror.ProductOf]:
    val primaryKey: PrimaryKey[Any, A]
    val table: Table
    def map(a: A): Tuple2[Iterator[Fragment], Iterator[Fragment]]

  case class Query(command: Command, table: Table, arguments: List[Argument])

  case class GenericCol[A](name: Fragment)
