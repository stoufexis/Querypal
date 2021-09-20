import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._

trait NonCompletableWhere[A, B <: Model[A], C <: ModelMeta[A]] {
  def where(f: B => Fragment): Condition[A, B, C]
}

final class Where[A, B <: Model[A], C <: ModelMeta[A]](
    contents: List[Fragment],
    model: B,
    meta: C
) extends Completable[A](contents)
    with NonCompletableWhere[A, B, C] {
  def where(f: B => Fragment): Condition[A, B, C] =
    Condition(contents ++ List(sql"where ") :+ f(model), model, meta)
}

final class Condition[A, B <: Model[A], C <: ModelMeta[A]](
    contents: List[Fragment],
    model: B,
    meta: C
) extends Completable[A](contents) {

  def and(f: B => Fragment): Condition[A, B, C] =
    Condition(contents ++ List(sql"and ") :+ f(model), model, meta)

  def or(f: B => Fragment): Condition[A, B, C] =
    Condition(contents ++ List(sql"or ") :+ f(model), model, meta)

  def bind(
      f: InternalCondition[A, B, C] => InternalCondition[A, B, C]
  ): Condition[A, B, C] =
    Condition(
      contents
        .dropRight(1)
        .appended(sql"( ")
        .concat(
          f(InternalCondition(List(contents.last), model, meta)).conditions
        )
        .appended(sql") "),
      model,
      meta
    )

}

final class InternalCondition[A, B <: Model[A], C <: ModelMeta[A]](
    val conditions: List[Fragment] = List(),
    model: B,
    meta: C
) {

  def and(f: B => Fragment): InternalCondition[A, B, C] =
    InternalCondition(
      conditions ++ List(sql"and ") :+ f(model),
      model,
      meta
    )

  def or(f: B => Fragment): InternalCondition[A, B, C] =
    InternalCondition(
      conditions ++ List(sql"or ") :+ f(model),
      model,
      meta
    )

}

object Where {
  def apply[A, B <: Model[A], C <: ModelMeta[A]](
      contents: List[Fragment],
      model: B,
      meta: C
  ) = new Where(
    contents,
    model,
    meta
  )
}

object Condition {}
