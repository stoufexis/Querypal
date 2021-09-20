import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._

trait NonCompletableWhere[A, B <: Model[A]] {
  def where(f: B => Fragment): Condition[A, B]
}

final class Where[A, B <: Model[A]](
    contents: List[Fragment],
    model: B,
    meta: ModelMeta[A]
) extends Completable[A](contents)
    with NonCompletableWhere[A, B] {
  def where(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"where ") :+ f(model), model, meta)
}

final class Condition[A, B <: Model[A]](
    contents: List[Fragment],
    model: B,
    meta: ModelMeta[A]
) extends Completable[A](contents) {

  def and(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"and ") :+ f(model), model, meta)

  def or(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"or ") :+ f(model), model, meta)

  def bind(
      f: Condition[A, B] => Condition[A, B]
  ): Condition[A, B] =
    Condition(
      contents
        .dropRight(1)
        .appended(sql"( ")
        .concat(
          List(f(Condition(List(), model, meta)).complete)
        )
        .appended(sql") "),
      model,
      meta
    )

}

object Where {
  def apply[A, B <: Model[A]](
      contents: List[Fragment],
      model: B,
      meta: ModelMeta[A]
  ) = new Where(
    contents,
    model,
    meta
  )
}

object Condition {}
