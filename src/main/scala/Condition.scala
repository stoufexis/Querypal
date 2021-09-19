import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._

trait NonCompletableWhere[A, B <: Model[A]] {
  def where(f: B => Fragment): Condition[A, B]
}

final class Where[A, B <: Model[A]](
    contents: List[Fragment],
    model: B
) extends Completable[A](contents)
    with NonCompletableWhere[A, B] {
  def where(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"where ") :+ f(model), model)
}

final class Condition[A, B <: Model[A]](
    contents: List[Fragment],
    model: B
) extends Completable[A](contents) {

  def and(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"and ") :+ f(model), model)

  def or(f: B => Fragment): Condition[A, B] =
    Condition(contents ++ List(sql"or ") :+ f(model), model)

  def bind(
      f: InternalCondition[A, B] => InternalCondition[A, B]
  ): Condition[A, B] =
    Condition(
      contents
        .dropRight(1)
        .appended(sql"( ")
        .concat(
          f(InternalCondition(List(contents.last), model)).conditions
        )
        .appended(sql") "),
      model
    )

}

final class InternalCondition[A, B <: Model[A]](
    val conditions: List[Fragment] = List(),
    model: B
) {

  def and(f: B => Fragment): InternalCondition[A, B] =
    InternalCondition(
      conditions ++ List(sql"and ") :+ f(model),
      model
    )

  def or(f: B => Fragment): InternalCondition[A, B] =
    InternalCondition(
      conditions ++ List(sql"or ") :+ f(model),
      model
    )

}

object Where {
  def apply[A, B <: Model[A]](
      contents: List[Fragment],
      model: B
  ) = new Where(
    contents,
    model
  )
}

object Condition {}
