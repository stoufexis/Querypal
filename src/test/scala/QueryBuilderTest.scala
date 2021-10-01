import org.scalatest.funsuite.AnyFunSuite
import logic.Model._
import logic.Relation._
import command.QueryBuilder
import scala.language.postfixOps
import logic.FragmentOperations._
import logic.FragmentOperations.{given, *}
import logic.FragmentOperations.{given, *}

case class Person(name: String, age: Int, nickname: String)
case class Photo(name: String, photographer: String)

object Person extends Model[Person]:
  val name     = column[String]("name")
  val age      = column[Int]("age")
  val nickname = column[String]("nickname")

  object Meta:
    given ModelMeta[Person] = deriveMeta("person", name, age, nickname)

object Photo extends Model[Photo]:
  val name         = column[String]("name")
  val photographer = column[String]("photographer_name")

  object Meta:
    given ModelMeta[Photo] = deriveMeta("photo", name, photographer)

import Person.Meta.{given, *}
import Photo.Meta.{given, *}
given Relation[Photo, Person](Photo.photographer)

class QueryBuilderTest extends AnyFunSuite {
  test("Select query construction") {
    assert(
      (QueryBuilder(Person)
        .select(_.age < 14)
        .or(_.nickname like "%e%")
        .bind(_ and (_.age < 30))
        .constructString) ==
        "select * from person where person.age < 14 or ( person.nickname like '%e%' and person.age < 30 )"
    )
  }

  test("Update query construction") {
    assert(
      (QueryBuilder(Person)
        .update(_.age set 13)
        .update(_.nickname set "Young Again")
        .where(_.age > 40)
        .constructString) ==
        "update person set age = 13 , nickname = 'Young Again' where person.age > 40"
    )
  }

  test("Join query construction") {
    assert(
      (QueryBuilder(Person)
        .select(_.age > 13)
        .or(_.age < 12)
        .bind(_ and (_.nickname like "%h%"))
        .join(Photo)
        .select(_.name like "%Selfie%")
        .constructString) ==
        "select * from person inner join photo on photo.photographer_name = person.name where person.age > 13 or ( person.age < 12 and person.nickname like '%h%' ) and photo.name like '%Selfie%'"
    )
  }

  test("Delete query construction") {
    assert(
      (QueryBuilder(Person)
        .delete(_.name like "Sam")
        .or(_.name like "Baily")
        .constructString) == "delete from person where person.name like 'Sam' or person.name like 'Baily'"
    )
  }
}
