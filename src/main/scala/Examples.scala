import Common._
import doobie.util.transactor.Transactor
import cats.effect.IO
import doobie.implicits._
import doobie.util.fragment.Fragment
import cats.effect.IOApp
import cats.effect.kernel.Resource.ExitCase
import cats.effect.ExitCode
import scala.language.postfixOps

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

val namef   = fr"name"
val phNamef = fr"photographer_name"

object Photo extends Model[Photo] {
  val name         = column[String](namef)
  val photographer = column[String](phNamef)
}

given ModelMeta[Photo] =
  deriveModelMeta[Photo](sql"photo")(namef)(phNamef)

case class Person(name: String, age: Int, nickname: String)

val agef      = fr"age"
val nicknamef = fr"nickname"

object Person extends Model[Person]:
  val name     = column[String](fr"name")
  val age      = column[Int](fr"age")
  val nickname = column[String](fr"nickname")

given ModelMeta[Person] =
  deriveModelMeta[Person](sql"person")(namef)(agef, nicknamef)

given Relation[Photo, Person](Photo.photographer)

object Main extends IOApp {
  import FragmentOperations.{given, *}

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    val insertPerson =
      QueryBuilder(Person) insert Person("Jack2", 34, "The kid") construct

    for
      res <- insertPerson.update.run.transact(xa)
      _   <- IO.println(res)
    yield ExitCode.Success

    val insertPerson1 = (person: Person) =>
      (QueryBuilder(Person) insert person construct).update.run.transact(xa)

    for
      res1 <- insertPerson1(Person("Dennis", 12, "The Menace"))
      res2 <- insertPerson1(Person("George", 56, "Angry neighbour"))
      res3 <- insertPerson1(Person("Karen", 48, "Angrier Neighbour"))
    yield ExitCode.Success

    // keep in mind, doobie requires your case class' fields to conform to the order of your db table's columns

    val selectAll = QueryBuilder(Person) select * construct

    for
      people <- selectAll.query[Person].to[List].transact(xa)
      _      <- IO.println(people)
    yield ExitCode.Success

    val set = QueryBuilder(
      Person
    ) update (_.age set 13) update (_.nickname set "Young Again") where (_.age > 45) construct

    for
      res <- set.update.run.transact(xa)
      _   <- IO.println(res)
    yield ExitCode.Success

    val del = QueryBuilder(
      Person
    ) delete (_.name === "Karen") or (_.name === "George") construct

    for
      aa <- del.update.run.transact(xa)
      _  <- IO.println(aa)
    yield ExitCode.Success

    IO(ExitCode.Success)

    val insertPhoto = (photo: Photo) =>
      (QueryBuilder(Photo) insert photo construct).update.run.transact(xa)

    for
      res1 <- insertPhoto(Photo("Day in the park", "Dennis"))
      res2 <- insertPhoto(Photo("Later that day in the same park", "Dennis"))
      res3 <- insertPhoto(Photo("Dennis in the park", "Jack"))
    yield ExitCode.Success

    val join = QueryBuilder(Person) join Photo select (_.age > 20) construct

    for
      aa <- join.query[(Person, Photo)].to[List].transact(xa)
      _  <- IO.println(aa)
    yield ExitCode.Success

    val query = QueryBuilder(
      Person
    ) select (_.age > 13) or (_.nickname like "The%") bind (_ and (_.age < 40)) construct

    for
      aa <- query.query[Person].to[List].transact(xa)
      _  <- IO.println(aa)
    yield ExitCode.Success

}
