import doobie.util.fragment.Fragment
import doobie.implicits._
import Common._
import cats.implicits._

final class Update[A, B <: Fields](
    contents: List[Fragment],
    model: Model[A, B]
) {
  def apply(
      f: (A => List[FieldValue]) => List[FieldValue]
  ): Where[A, B] =
    Where(
      contents ++ (sql"set " +:
        f(model.meta.mapper).flatMap { case FieldValue(field, value) =>
          List(field.name, sql" = ", value, sql", ")
        }).dropRight(1) :+ sql" ",
      model
    )
}

object Update {
  def apply[A, B <: Fields](
      contents: List[Fragment],
      model: Model[A, B]
  ) =
    new Update(contents, model)

}
