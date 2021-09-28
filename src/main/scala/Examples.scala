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

import doobie.util.log.LogHandler
import doobie.util.update
import DeriveModelMeta.deriveModelMeta

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
  val owner = column[Int]("owner_name")

  object Meta:
    given ModelMeta[Pet] = deriveMeta(name, owner)

import Photo.Meta.{given, *}
import Person.Meta.{given, *}
import Pet.Meta.{given, *}

given Relation[Photo, Person](Photo.photographer)

given Relation[Pet, Person](Pet.owner)

object Main extends IOApp {
  import FragmentOperations.{given, *}

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    val del = QueryBuilder(Person) select (_.age > 20) join
      Photo select (_.name like "%Bob") join Pet select (_.name like "G%") construct

    for
      // aa <- del.query[(Person, Photo)].to[List].transact(xa)
      _ <- IO.println(del)
    yield ExitCode.Success

}
