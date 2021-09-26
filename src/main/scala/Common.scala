import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import scala.deriving.Mirror
import Common.ModelMeta
import scala.compiletime.summonInline

object Common:

  /** The trait to be extended by the model of A. It provides helpful type
    * inference in other parts of the pipeline and some helpful methods to
    * construct the model
    */
  trait Model[A]:
    protected def column[B](name: Fragment): Column[A, B] = Column(name)
    protected def foreignKey[B, C](field: Field[B, C])(
        name: Fragment
    ): ForeignKey[A, B, C] = ForeignKey(name)

  /** A type class applied to A, containing metadata about the model of A.
    */
  trait ModelMeta[A]:
    val primaryKeyName: Fragment
    val table: Table

    /** Deconstructs and instance of A so it can be used in queries
      */
    def map(a: A): (Iterator[Fragment], Iterator[Fragment])

  /** The query in its preconstructed form
    */
  case class Query(command: Command, table: Table, arguments: List[Argument])

  /** A field of A modeling a value member of type B
    */
  sealed trait Field[A, B]:
    val name: Fragment
  case class Column[A, B](name: Fragment)        extends Field[A, B]
  case class ForeignKey[A, B, C](name: Fragment) extends Field[A, B]

  case class PrimaryKey[A, B](field: Field[A, B])

  /** A type level declaration of a Relation between A and B, used to validate
    * the existence of a relation during compile time
    */
  trait Relation[A, B](from: Field[?, A])(using
      fromMeta: ModelMeta[A],
      toMeta: ModelMeta[B]
  ):
    val joinCondition: Argument =
      fromMeta.table.name ++ sql"." ++ from.name ++ fr"=" ++ toMeta.table.name ++ sql"." ++ toMeta.primaryKeyName
