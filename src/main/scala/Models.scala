import logic.Relation._
import logic.Model._

object Models:
  import Photo.Meta.{given, *}
  import Person.Meta.{given, *}
  import Pet.Meta.{given, *}

  case class Person(name: String, age: Int, nickname: String)
  case class Photo(name: String, photographer: String)
  case class Pet(name: String, owner: String)

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

  object Pet extends Model[Pet]:
    val name  = column[String]("name")
    val owner = column[String]("owner_name")

    object Meta:
      given ModelMeta[Pet] = deriveMeta("pet", name, owner)

  given Relation[Photo, Person](Photo.photographer)
  given Relation[Pet, Person](Pet.owner)
