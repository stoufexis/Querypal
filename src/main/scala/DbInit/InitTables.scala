package DbInit

import doobie.implicits._
import doobie.util.transactor.Transactor
import cats.effect.IO
import cats.effect.ExitCode
import cats.effect.IOApp
import doobie.util.log.LogHandler
import cats.implicits._
import cats.Apply
import cats.kernel.Semigroup
import cats.kernel.Monoid

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

object InitTables extends IOApp {

  val drop =
    sql"""
    DROP TABLE IF EXISTS photo
  """.update.run

  val create =
    sql"""
    CREATE TABLE photo (
      name              varchar not null,
      photographer_name varchar not null
    )
  """.update.run

  val primaryKey =
    sql"""
    ALTER TABLE photo ADD CONSTRAINT photo_pk PRIMARY KEY (name);    
  """.update.run

  val foreignKey =
    sql"""
    ALTER TABLE photo ADD FOREIGN KEY (photographer_name) REFERENCES person(name);    
  """.update.run

  val index =
    sql"""
      create unique index photo_name_uindex on photo (name);
    """.update.run

  def run(args: List[String]): IO[ExitCode] = {
    implicit val han = LogHandler.jdkLogHandler

    implicit def semigroup[F[_]: Apply, A: Monoid]: Semigroup[F[A]] =
      Apply.semigroup[F, A]

    val dddd = (drop |+| create |+| primaryKey |+| foreignKey |+| index)
    for asd <- dddd.transact(xa)
    yield ExitCode.Success
  }

}
