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
  val name         = Column[String, Photo](fr"name")
  val photographer = Column[String, Photo](fr"photographer_name")
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
  val pk                    = PrimaryKey(PhotoFields.name)
  def mapper(entity: Photo) = List()
}

case class Person(name: String, age: Int)

given personModel: Model[Person] with {
  val name = Column[String, Person](fr"name")
  val age  = Column[Int, Person](fr"age")
}

given ModelMeta[Person](sql"person") with
  val pk = PrimaryKey(personModel.name)
  def mapper(entity: Person) =
    List(
      (personModel.name -> fr"${entity.name}"),
      (personModel.age  -> fr"${entity.age}")
    )

given ManyToOne[Photo, Person](PhotoFields.photographer)

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    implicit val han = LogHandler.jdkLogHandler

    // for
    //   age <- IO(
    //     QueryBuilder(
    //       personModel
    //     ) insert Person("Person2", 34) construct
    //   )
    //   aa <- age.update.run.transact(xa)
    //   _  <- IO.println(aa)
    // yield ExitCode.Success

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

    for
      age <- IO(
        QueryBuilder(
          personModel
        ) select (_.age > 14) or (_.age === 13) bind (_ and (_.name === "jak")) construct
      )
      aa <- age.query[Person].to[List].transact(xa)
      _  <- IO.println(aa)
    yield ExitCode.Success

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
