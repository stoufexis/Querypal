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

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

case class Photo(name: String, photographer: String)

// case object PhotoFields extends Fields {
//   val name         = Field[String](fr"name")
//   val photographer = Field[String](fr"photographer_name")
// }

// case object PhotoMeta extends ModelMeta[Photo] {
//   val table = fr"person"

//   val pk          = PrimaryKey(PhotoFields.name)
//   override val fk = Some(ForeignKey(PhotoFields.photographer, PersonModel))

//   def mapper(entity: Photo): List[FieldValue] =
//     List(
//       FieldValue(PhotoFields.name, fr"${entity.name}"),
//       FieldValue(PhotoFields.photographer, fr"${entity.photographer}")
//     )
// }

case class Person(name: String, age: Int)

case object PersonFields extends Fields[Person] {
  val name = PrimaryKey[String, Person](fr"name")
  val age  = Column[Int, Person](fr"age")
}

given ModelMeta[Person] with
  val table = Table(fr"person")
  def mapper(entity: Person) =
    List(
      (PersonFields.name -> fr"${entity.name}"),
      (PersonFields.age  -> fr"${entity.age}")
    )

val PersonModel = Model(PersonFields)

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    //   for
    //     people <- QueryBuilder(PersonModel).select
    //       .where(_.name eqls "Gordon")
    //       .complete
    //       .query[Person]
    //       .to[List]
    //       .transact(xa)
    //     _ <- IO(people.foreach(println(_)))
    //   yield ExitCode.Success
    // for
    //   query <- IO(
    //     QueryBuilder(PersonModel)
    //       .insert(_(Person("Jack3", 14)))
    //       .complete
    //   )
    //   _ <- IO.println(query)
    //   _ <- query.update.run
    //     .transact(xa)
    // yield ExitCode.Success
    // for
    //   query <- IO(
    //     QueryBuilder(PersonModel).delete.where(_.age gt 14).complete
    //   )
    //   _ <- IO.println(query)
    //   _ <- query.update.run
    //     .transact(xa)
    // yield ExitCode.Success
    implicit val han = LogHandler.jdkLogHandler

    // for
    //   query <- IO(
    //     QueryBuilder(
    //       PersonModel
    //     ).update set (_.age === 13) set (_.name === "unknown") where (_.age > 14) construct
    //   )

    //   _ <- query.update.run.transact(xa)
    // yield ExitCode.Success

    // for
    //   query <- IO(
    //     QueryBuilder(
    //       PersonModel
    //     ).select where (_.age > 10) bind (_ or (_.age < 5)) construct
    //   )

    //   _ <- query.update.run.transact(xa)
    // yield ExitCode.Success

    // def asd[A, B <: Product](entity: A, entityRef: B) = {}

    for
      query <- IO(
        QueryBuilder(
          PersonModel
        ).select where (_.age > 13) construct
      )

      _ <- query.update.run.transact(xa)
    yield ExitCode.Success

    for
      query <-
        sql"""select * from person left outer join photo on person.name = photo.photographer_name where (person.name = 'Stef' or person.name = 'Stef2')"""
          .query[(Person, Option[Photo])]
          .to[List]
          .transact(xa)
      _ <- IO.println(query)
    yield ExitCode.Success

  val Sell = "sell"
  val Buy  = "buy"

}
