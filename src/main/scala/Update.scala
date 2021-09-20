import doobie.util.fragment.Fragment
import doobie.implicits._
import Common._
import cats.implicits._

final class Update[A, B <: Model[A]](
    contents: List[Fragment],
    model: B,
    meta: ModelMeta[A]
) {
  def apply(
      f: (A => List[FieldValue]) => List[FieldValue]
  ): NonCompletableWhere[A, B] =
    Where(
      contents ++ (sql"set " +:
        f(meta.mapper).flatMap { case FieldValue(field, value) =>
          List(field.name, sql" = ", value, sql", ")
        }).dropRight(1) :+ sql" ",
      model,
      meta
    )

}

object Update {
  def apply[A, B <: Model[A]](
      contents: List[Fragment],
      model: B,
      meta: ModelMeta[A]
  ) =
    new Update(contents, model, meta)

}
