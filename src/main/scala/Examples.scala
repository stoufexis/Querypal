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
import db_init._
import command._

import doobie.util.log.LogHandler
import doobie.util.update
import cats.Apply
import cats.kernel.Semigroup
import logic.Model._

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

import Models.Photo.Meta.{given, *}
import Models.Person.Meta.{given, *}
import Models.Pet.Meta.{given, *}
import Models._

object InitDB {
  import InitTables._

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    given [F[_]: Apply, A: Semigroup]: Semigroup[F[A]] =
      Apply.semigroup[F, A]

    val quer =
      (QueryBuilder(Person) insert Person(
        "Jack",
        13,
        "Ugly"
      ) construct).update.run

    val quer1 =
      (QueryBuilder(Person) insert Person(
        "John",
        23,
        "Pretty"
      ) construct).update.run

    val quer2 =
      (QueryBuilder(Person) insert Person(
        "Sam",
        33,
        "Mayhem"
      ) construct).update.run

    val quer3 =
      (QueryBuilder(Person) insert Person(
        "Baily",
        43,
        "Satan"
      ) construct).update.run

    val quer4 =
      (QueryBuilder(Photo) insert Photo(
        "Jack Selfie",
        "Jack"
      ) construct).update.run

    val quer5 =
      (QueryBuilder(Photo) insert Photo(
        "John Selfie",
        "John"
      ) construct).update.run

    val quer6 =
      (QueryBuilder(Photo) insert Photo(
        "Johns House",
        "John"
      ) construct).update.run

    val quer7 =
      (QueryBuilder(Pet) insert Pet(
        "Sammy",
        "Jack"
      ) construct).update.run

    val quer8 =
      (QueryBuilder(Pet) insert Pet(
        "Satan",
        "Baily"
      ) construct).update.run

    val genTables =
      (tableGen[Person]
        |+| tableGen[Photo]
        |+| tableGen[Pet]
        |+| relationGen[Photo, Person]
        |+| relationGen[Pet, Person]
        |+| quer |+| quer1 |+| quer2 |+| quer3 |+| quer4 |+| quer5 |+| quer6 |+| quer7 |+| quer8)

    for _ <- genTables.transact(xa)
    yield ExitCode.Success

}

object Main extends IOApp {
  import logic.FragmentOperations._
  import logic.FragmentOperations.{given, *}

  given [F[_]: Apply, A: Semigroup]: Semigroup[F[A]] =
    Apply.semigroup[F, A]

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    val multiJoin = QueryBuilder(Person) select
      (_.age > 13) or (_.age < 12) bind (_ and (_.nickname like "%h%")) join
      Photo select (_.name like "%Selfie%") construct

    for
      res <- multiJoin.query[(Person, Photo)].to[List].transact(xa)
      _   <- IO.println(res)
    yield ExitCode.Success

}
