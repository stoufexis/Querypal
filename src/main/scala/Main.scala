import Common._
import doobie.util.transactor.Transactor
import cats.effect.IO
import doobie.implicits._
import doobie.util.fragment.Fragment
import cats.effect.IOApp
import cats.effect.kernel.Resource.ExitCase
import cats.effect.ExitCode
import org.checkerframework.checker.units.qual.s

val xa = Transactor.fromDriverManager[IO](
  "org.postgresql.Driver",                     // driver classname
  "jdbc:postgresql://localhost:5432/postgres", // connect URL (driver-specific)
  "postgres",                                  // user
  "postgres"                                   // password
)

case class Person(name: String, age: Int)

object PersonModel extends Model[Person] {
  val name: Field[String] = Field(sql"name")
  val age: Field[Int]     = Field(sql"age")
  val table               = sql"person"

  val toTuples = (entity: Person) =>
    List(
      FieldValue(name, sql"${entity.name}"),
      FieldValue(age, sql"${entity.age}")
    )
}
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
    //       .insert(_(Person("Jack2", 14)))
    //       .complete
    //   )
    //   _ <- IO.println(query)
    //   _ <- query.update.run
    //     .transact(xa)
    // yield ExitCode.Success
    for
      query <- IO(
        QueryBuilder(PersonModel).delete.where(_.age gt 14).complete
      )
      _ <- IO.println(query)
      _ <- query.update.run
        .transact(xa)
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
