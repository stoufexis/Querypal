import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._

trait Where[A, B <: Fields] {
  def where(f: B => Fragment): Condition[A, B] & Completable
}

final class WhereImpl[A, B <: Fields](
    contents: List[Fragment],
    model: Model[A, B]
) extends Where[A, B]
    with Completable(contents) {
  def where(f: B => Fragment): Condition[A, B] & Completable =
    Condition(contents ++ List(sql"where ") :+ f(model.fields), model)
}

object Where {
  def apply[A, B <: Fields](
      contents: List[Fragment],
      model: Model[A, B]
  ) = new WhereImpl(
    contents,
    model
  )
}
