import Common._
import doobie.util.transactor.Transactor
import cats.effect.IO
import doobie.implicits._
import doobie.util.fragment.Fragment
import cats.effect.IOApp
import cats.effect.kernel.Resource.ExitCase
import cats.effect.ExitCode
import scala.language.postfixOps
import doobie.util.update.Update0
import cats.implicits._

import doobie.util.log.LogHandler
import doobie.util.update
import DeriveModelMeta.deriveModelMeta
import cats.Apply
import cats.kernel.Semigroup

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

case class Photo(name: String, photographer: String)

object Photo extends Model[Photo]("photo"):
  val name         = column[String]("name")
  val photographer = column[String]("photographer_name")

  object Meta:
    given ModelMeta[Photo] = deriveMeta(name, photographer)

given Relation[Photo, Person](Photo.photographer)

case class Person(name: String, age: Int, nickname: String)

object Person extends Model[Person]("person"):
  val name     = column[String]("name")
  val age      = column[Int]("age")
  val nickname = column[String]("nickname")

  object Meta:
    given ModelMeta[Person] = deriveMeta(name, age, nickname)

case class Pet(name: String, owner: String)

object Pet extends Model[Pet]("pet"):
  val name  = column[String]("name")
  val owner = column[String]("owner_name")

  object Meta:
    given ModelMeta[Pet] = deriveMeta(name, owner)

given Relation[Pet, Person](Pet.owner)

import Photo.Meta.{given, *}
import Person.Meta.{given, *}
import Pet.Meta.{given, *}

object CreateTables {
  import InitTables._

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    given [F[_]: Apply, A: Semigroup]: Semigroup[F[A]] =
      Apply.semigroup[F, A]

    val genTables = (tableGen[Person] |+| tableGen[Photo] |+| tableGen[Pet]
      |+| relationGen[Photo, Person] |+| relationGen[Pet, Person])

    for _ <- genTables.transact(xa)
    yield ExitCode.Success

}

object Main extends IOApp {
  import FragmentOperations.{given, *}

  given [F[_]: Apply, A: Semigroup]: Semigroup[F[A]] =
    Apply.semigroup[F, A]

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    val query = QueryBuilder(
      Person
    ) select (_.age > 13) or (_.age < 12) bind (_ and (_.nickname like "%")) join
      Photo select (_.name like "%Selfie%") join Pet select (_.name like "%%") construct

    for
      // aa <- multiJoin.query[(Person, Photo, Pet)].to[List].transact(xa)
      _ <- IO.println(query)
    yield ExitCode.Success

}
