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
  val photographer = column[String]("photographer")

  object Meta:
    given ModelMeta[Photo] = deriveMeta

case class Person(name: String, age: Int, nickname: String)

object Person extends Model[Person]("person"):
  val name     = column[String]("name")
  val age      = column[Int]("age")
  val nickname = column[String]("nickname")

  object Meta:
    given ModelMeta[Person] = deriveMeta

import Photo.Meta.{given, *}
import Person.Meta.{given, *}

given Relation[Photo, Person](Photo.photographer)

object Main extends IOApp {
  import FragmentOperations.{given, *}

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    val insertPerson =
      QueryBuilder(Person) insert Person("Jack10", 34, "The kid") construct

    for _ <- insertPerson.update.run.transact(xa)
    yield ExitCode.Success

}
