import doobie.util.fragment.Fragment

case class FieldValue(field: Field[Any], value: Fragment)

object Common {
  import cats.Monoid
  import cats.implicits._

  trait Model[B] {
    val table: Fragment
    val toTuples: (entity: B) => List[FieldValue]
  }

  trait Completable[A](
      content: List[Fragment]
  ) {
    def complete: Fragment = content.fold(Monoid[Fragment].empty)(_ combine _)
  }
}
