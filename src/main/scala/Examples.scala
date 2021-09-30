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
  import logic.FragmentOperations._
  import logic.FragmentOperations.{given, *}

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
