import FragmentOperations._
import scala.deriving.Mirror
import Common.ModelMeta
import scala.compiletime.{summonInline, constValue}
import DeriveModelMeta.deriveModelMeta

object Common:

  trait ToTypeDescription[A] {
    def toTypeDescription(name: String): String
  }

  given ToTypeDescription[String] with {
    def toTypeDescription(name: String): String = s"$name varchar not null"
  }

  given ToTypeDescription[Int] with {
    def toTypeDescription(name: String): String = s"$name integer not null"
  }

  /** The trait to be extended by the model of A. It provides helpful type
    * inference in other parts of the pipeline and some helpful methods to
    * construct the model
    */
  trait Model[A](val tableName: String):
    protected inline def deriveMeta(fields: Field[?, ?]*)(using
        m: Mirror.ProductOf[A]
    ) = deriveModelMeta[m.MirroredMonoType](tableName)(fields)

    protected def column[B: ToTypeDescription](name: String): Column[A, B] =
      Column(name)

  /** A type class applied to A, containing metadata about the model of A.
    */
  trait ModelMeta[A]:
    val primaryKeyName: String
    val table: Table
    val typeDescriptions: Seq[String]

    /** Deconstructs and instance of A so it can be used in queries
      */
    def map(a: A): (Seq[String], Seq[String])

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
    val toTypeDescription: String
  case class Column[A, B](name: String)(using toType: ToTypeDescription[B])
      extends Field[A, B]:
    override val toString: String = name
    val toTypeDescription: String = toType.toTypeDescription(name)

  case class PrimaryKey[A, B](field: Field[A, B])

  /** A type level declaration of a Relation between A and B, used to validate
    * the existence of a relation during compile time
    */
  trait Relation[A, B](val fk: Field[A, ?])(using
      fromMeta: ModelMeta[A],
      toMeta: ModelMeta[B]
  ):
    val joinCondition: String =
      fromMeta.table.name + s"." + fk.toString ++ s" = " + toMeta.table.name ++ s"." + toMeta.primaryKeyName
