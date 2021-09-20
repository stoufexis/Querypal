import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._

trait Condition[A, B <: Fields] {
  def and(f: B => Fragment): Condition[A, B] & Completable

  def or(f: B => Fragment): Condition[A, B] & Completable

  def bind(
      f: Condition[A, B] => Condition[A, B] & Completable
  ): Condition[A, B] & Completable
}

final class ConditionImpl[A, B <: Fields](
    contents: List[Fragment],
    model: Model[A, B]
) extends Condition[A, B]
    with Completable(contents) {

  def and(f: B => Fragment): Condition[A, B] & Completable =
    Condition(contents ++ List(sql"and ") :+ f(model.fields), model)

  def or(f: B => Fragment): Condition[A, B] & Completable =
    Condition(contents ++ List(sql"or ") :+ f(model.fields), model)

  def bind(
      f: Condition[A, B] => Condition[A, B] & Completable
  ): Condition[A, B] & Completable =
    Condition(
      contents
        .dropRight(1)
        .appended(sql"( ")
        .concat(
          List(f(Condition(List(), model)).complete)
        )
        .appended(sql") "),
      model
    )

}

object Condition {
  def apply[A, B <: Fields](
      contents: List[Fragment],
      model: Model[A, B]
  ) = new ConditionImpl(
    contents,
    model
  )
}
