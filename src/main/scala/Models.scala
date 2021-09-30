import logic.Relation._
import logic.Model._

object Models:
  import Photo.Meta.{given, *}
  import Person.Meta.{given, *}
  import Pet.Meta.{given, *}

  case class Photo(name: String, photographer: String)

  object Photo extends Model[Photo]("photo"):
    val name         = column[String]("name")
    val photographer = column[String]("photographer_name")

    object Meta:
      given ModelMeta[Photo] = deriveMeta(name, photographer)

  given Relation[Photo, Person](Photo.photographer)

  case class Person(name: String, age: Int, nickname: String)

  object Person extends Model[Person]("person"):
    val name     = column[String]("name")
    val age      = column[Int]("age")
    val nickname = column[String]("nickname")

    object Meta:
      given ModelMeta[Person] = deriveMeta(name, age, nickname)

  case class Pet(name: String, owner: String)

  object Pet extends Model[Pet]("pet"):
    val name  = column[String]("name")
    val owner = column[String]("owner_name")

    object Meta:
      given ModelMeta[Pet] = deriveMeta(name, owner)

  given Relation[Pet, Person](Pet.owner)
