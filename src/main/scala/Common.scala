import FragmentOperations._
import scala.deriving.Mirror
import Common.ModelMeta
import scala.compiletime.{summonInline, constValue}
import DeriveModelMeta.deriveModelMeta

object Common:

  /** The trait to be extended by the model of A. It provides helpful type
    * inference in other parts of the pipeline and some helpful methods to
    * construct the model
    */
  trait Model[A](val tableName: String):
    protected inline def deriveMeta(fields: Field[?, ?]*)(using
        m: Mirror.ProductOf[A]
    ) =
      deriveModelMeta[m.MirroredMonoType](tableName)(fields.map(_.name))
    protected def column[B](name: String): Column[A, B] = Column(name)

  /** A type class applied to A, containing metadata about the model of A.
    */
  trait ModelMeta[A]:
    val primaryKeyName: String
    val table: Table

    /** Deconstructs and instance of A so it can be used in queries
      */
    def map(a: A): (Iterator[String], Iterator[String])

  /** The query in its preconstructed form
    */
  case class Query(
      command: Command,
      table: Table,
      arguments: List[Argument],
      joins: List[JoinArgument] = List()
  )

  /** A field of A modeling a value member of type B
    */
  sealed trait Field[A, B]:
    val name: String
  case class Column[A, B](name: String)        extends Field[A, B]
  case class ForeignKey[A, B, C](name: String) extends Field[A, B]

  case class PrimaryKey[A, B](field: Field[A, B])

  /** A type level declaration of a Relation between A and B, used to validate
    * the existence of a relation during compile time
    */
  trait Relation[A, B](from: Field[A, ?])(using
      fromMeta: ModelMeta[A],
      toMeta: ModelMeta[B]
  ):
    val joinCondition: String =
      fromMeta.table.name + s"." + from.name ++ s" = " + toMeta.table.name ++ s"." + toMeta.primaryKeyName
