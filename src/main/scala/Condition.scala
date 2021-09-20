import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._

trait Where[A, B <: Fields] {
  def where(f: B => Fragment): Condition[A, B]
}

final class WhereImpl[A, B <: Fields](
    contents: List[Fragment],
    model: Model[A, B]
) extends Completable[A](contents) {
  def where(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"where ") :+ f(model.fields), model)
}

final class Condition[A, B <: Fields](
    contents: List[Fragment],
    model: Model[A, B]
) extends Completable[A](contents) {

  def and(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"and ") :+ f(model.fields), model)

  def or(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"or ") :+ f(model.fields), model)

  def bind(
      f: Condition[A, B] => Condition[A, B]
  ): Condition[A, B] =
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

object Where {
  def apply[A, B <: Fields](
      contents: List[Fragment],
      model: Model[A, B]
  ) = new WhereImpl(
    contents,
    model
  )
}

object Condition {}
