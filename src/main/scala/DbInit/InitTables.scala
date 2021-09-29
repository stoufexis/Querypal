import Common.ModelMeta
import doobie.util.fragment.Fragment
import doobie.util.update.Update0
import cats.Apply
import cats.kernel.Monoid
import cats.kernel.Semigroup
import cats.implicits._
import doobie.ConnectionIO
import Common.Relation

object InitTables:

  def dropTableFr(tableName: String): Fragment =
    Update0(s"DROP TABLE IF EXISTS $tableName", None).toFragment

  def createTableFr(
      tableName: String,
      typeDescriptions: Seq[String]
  ): Fragment =
    val foldedTypeDescr = typeDescriptions.fold("")(_ + ", " + _).drop(2)
    Update0(s"CREATE TABLE $tableName ($foldedTypeDescr)", None).toFragment

  def addPrimaryKey(tableName: String, pkName: String) =
    Update0(
      s"ALTER TABLE $tableName ADD CONSTRAINT ${tableName}_pk PRIMARY KEY ($pkName)",
      None
    ).toFragment

  def addIndex(tableName: String, pkName: String) =
    Update0(
      s"create unique index ${tableName}_${pkName}_uindex on ${tableName}(${pkName})",
      None
    ).toFragment

  def addForeignKey(
      tableName: String,
      fkName: String,
      referencedTableName: String,
      referencedPkName: String
  ) = Update0(
    s"""ALTER TABLE $tableName 
        ADD FOREIGN KEY ($fkName) 
        REFERENCES $referencedTableName($referencedPkName)
    """,
    None
  ).toFragment

  implicit def semigroup[F[_]: Apply, A: Semigroup]: Semigroup[F[A]] =
    Apply.semigroup[F, A]

  def tableGen[A](using meta: ModelMeta[A]): ConnectionIO[Int] =
    val tableName = meta.table.name
    val genTableSteps = List(
      dropTableFr(tableName),
      createTableFr(tableName, meta.typeDescriptions),
      addPrimaryKey(tableName, meta.primaryKeyName),
      addIndex(tableName, meta.primaryKeyName)
    ).map(_.update.run)

    genTableSteps.drop(1).fold(genTableSteps.head)(_ |+| _)

  def relationGen[A, B](using
      meta: ModelMeta[A],
      meta2: ModelMeta[B],
      relation: Relation[A, B]
  ): ConnectionIO[Int] = addForeignKey(
    meta.table.name,
    relation.fk.toString,
    meta2.table.name,
    meta2.primaryKeyName
  ).update.run
