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

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

case class Photo(name: String, photographer: String)

case object PhotoFields {
  val name         = PrimaryKey[String](fr"name")
  val photographer = Column[String](fr"photographer_name")
}

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

given ModelMeta[Photo](sql"photo") with {
  def mapper(entity: Photo) = List()
}

case class Person(name: String, age: Int)

given personModel: Model[Person] with {
  val name = PrimaryKey[String](fr"name")
  val age  = Column[Int](fr"age")
}

given ModelMeta[Person](sql"person") with
  def mapper(entity: Person) =
    List(
      (personModel.name -> fr"${entity.name}"),
      (personModel.age  -> fr"${entity.age}")
    )

given ManyToOne[Photo, Person](PhotoFields.photographer) with {}

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
    implicit val han = LogHandler.jdkLogHandler
    for
      age <- IO(
        QueryBuilder(personModel)
          .join[Photo] construct
      )
      aa <- age.query[(Person, Photo)].to[List].transact(xa)
      _  <- IO.println(aa)
    yield ExitCode.Success

  //   _ <- query.update.run.transact(xa)
  // yield ExitCode.Success
  //sql"""select * from person inner join photo on person.name = photo.photographer_name where person.name = 'Stef'"""
  // for
  //   query <- IO(
  //     QueryBuilder(PersonModel).join[Photo] on (_.)
  //   )
  // .query[(Person, Option[Photo])]
  // .to[List]
  // .transact(xa)
  // _ <- IO.println(query)
  // yield ExitCode.Success

}
