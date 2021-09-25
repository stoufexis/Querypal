import Common._
import doobie.util.transactor.Transactor
import cats.effect.IO
import doobie.implicits._
import doobie.util.fragment.Fragment
import cats.effect.IOApp
import cats.effect.kernel.Resource.ExitCase
import cats.effect.ExitCode
import FragmentOperations._
import scala.language.postfixOps
import reflect.Selectable.reflectiveSelectable

import doobie.util.log.LogHandler
import scala.deriving.Mirror
import scala.deriving.Mirror.ProductOf
import scala.util.Random
import doobie.util.update
import DeriveModelMeta.deriveModelMeta

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

case class Photo(name: String, photographer: String)

case object PhotoFields {
  val name         = Column[String, Photo](fr"name")
  val photographer = Column[String, Photo](fr"photographer_name")
}

given ModelMeta[Photo] =
  deriveModelMeta[Photo](fr"photo")(PhotoFields.name)(PhotoFields.photographer)

case class Person(name: String, age: Int)

case object PersonModel extends Model[Person] {
  val name = Column[String, Person](fr"name")
  val age  = Column[Int, Person](fr"age")
}
given ModelMeta[Person] =
  deriveModelMeta[Person](fr"person")(PersonModel.name)(PersonModel.age)

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    for
      age <- IO(
        QueryBuilder(
          PersonModel
        ) insert Person("Person345", 34) construct
      )
    // age <- age.update.run.transact(xa)
    yield ExitCode.Success

  // val searchWord = "tef2"

  // for
  //   age <- IO(
  //     QueryBuilder(
  //       personModel
  //     ).join[Photo]
  //       select (_.name === "unknown") or (_.name like s"%${searchWord}") construct
  //   )
  //   aa <- age.query[(Person, Photo)].to[List].transact(xa)
  //   _  <- IO.println(aa)
  // yield ExitCode.Success

  // for
  //   age <- IO(
  //     QueryBuilder(
  //       personModel
  //     ) select (_.age > 14) or (_.age === 13) bind (_ and (_.name === "jak")) construct
  //   )
  //   aa <- age.query[Person].to[List].transact(xa)
  //   _  <- IO.println(aa)
  // yield ExitCode.Success

  // for
  //   age <- IO(
  //     QueryBuilder(
  //       personModel
  //     ) update (_.age set 13) update (_.name set "Re-aged123") where (_.name === "asdd") construct
  //   )
  //   aa <- age.update.run.transact(xa)
  //   _  <- IO.println(aa)
  // yield ExitCode.Success

  // for
  //   age <- IO(
  //     QueryBuilder(
  //       personModel
  //     ) delete (_.age > 29) or (_.name === "unknown") construct
  //   )
  //   aa <- age.update.run.transact(xa)
  //   _  <- IO.println(aa)
  // yield ExitCode.Success

}
