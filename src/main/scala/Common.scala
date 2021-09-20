import doobie.util.fragment.Fragment

case class FieldValue(field: Field[Any], value: Fragment)

object Common {
  import cats.Monoid
  import cats.implicits._

  trait Fields

  trait ModelMeta[B] {
    val table: Fragment
    def mapper(entity: B): List[FieldValue]
  }

  case class Model[A, B <: Fields](fields: B, meta: ModelMeta[A])

  trait Completable(
      val contents: List[Fragment]
  ) {
    def complete: Fragment = contents.fold(Monoid[Fragment].empty)(_ combine _)

  }
}
