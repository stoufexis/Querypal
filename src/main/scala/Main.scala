import Common._
import doobie.util.transactor.Transactor
import cats.effect.IO
import doobie.implicits._
import doobie.util.fragment.Fragment
import cats.effect.IOApp
import cats.effect.kernel.Resource.ExitCase
import cats.effect.ExitCode
import FragmentOperations._

import FragmentOperations.FieldOps._
import doobie.util.log.LogHandler

val xa = Transactor.fromDriverManager[IO](
  "org.postgrefr.Driver",                     // driver classname
  "jdbc:postgrefr://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                 // user
  "postgres"                                  // password
)

case class Person(name: String, age: Int)

case class Photo(name: String, photographer: String)

case object PhotoFields extends Fields {
  val name         = Field[String](fr"name")
  val photographer = Field[String](fr"photographerName")
}

case object PhotoMeta extends ModelMeta[Photo] {
  val table = fr"person"

  val pk          = PrimaryKey(PhotoFields.name)
  override val fk = Some(ForeignKey(PhotoFields.photographer, PersonModel))

  def mapper(entity: Photo): List[FieldValue] =
    List(
      FieldValue(PhotoFields.name, fr"${entity.name}"),
      FieldValue(PhotoFields.photographer, fr"${entity.photographer}")
    )
}

case object PersonFields extends Fields {
  val name: Field[String] = Field(fr"name")
  val age: Field[Int]     = Field(fr"age")
}

case object PersonMeta extends ModelMeta[Person] {
  val table = fr"person "
  val pk    = PrimaryKey(PersonFields.name)

  def mapper(entity: Person): List[FieldValue] =
    List(
      FieldValue(PersonFields.name, fr"${entity.name}"),
      FieldValue(PersonFields.age, fr"${entity.age}")
    )
}
val PersonModel = Model(PersonFields, PersonMeta)

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

    for
      query <- IO(
        QueryBuilder(PersonModel).delete
          .where(_.age lt 15)
          .or(_.name eqls "Jok2")
          .construct
      )

      _ <- IO.println(query.toString)
      _ <- query.update.run.transact(xa)
    yield ExitCode.Success
  // val query2 =
  //   QueryBuilder(PersonModel)
  //     .insert(model => (model.age value 14) and (model.name value "aasd"))
  //     .complete

  // val query3 =
  //   QueryBuilder(PersonModel).delete
  //     .where(_.age gt 13)
  //     .complete

  // val query4 =
  //   QueryBuilder(PersonModel).select.complete

  // val query5 =
  //   QueryBuilder(PersonModel)
  //     .update(model => (model.age set 13) and (model.name set "asdasd"))
  //     .where(_.age gt 15)
  //     .or(_.name eqls "AAAAA")
  //     .complete

  // println(query2)
  // println(query3)
  // println(query4)
  // println(query5)
}
