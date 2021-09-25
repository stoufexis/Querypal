import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import scala.deriving.Mirror
import Common.ModelMeta

object Common:

  type FieldValue[A] = (Field[?, A], Fragment)

  trait Model[A]

  trait ModelMeta[A: Mirror.ProductOf: ModelMeta]:
    val primaryKey: PrimaryKey[Any, A]
    val table: Table
    def map(a: A): Tuple2[Iterator[Fragment], Iterator[Fragment]]

  case class Query(command: Command, table: Table, arguments: List[Argument])

  case class FragmentBox[A](a: A):
    val getFragment = a match
      case x: Int    => sql"${x: Int}"
      case x: String => sql"${x: String}"
      case _         => sql""

  def toFragments[A <: Product: Mirror.ProductOf](entity: A) =
    Tuple.fromProductTyped(entity).map([T] => (a: T) => FragmentBox[T](a))
