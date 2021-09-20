import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment

trait Insert[A, B <: Fields] {
  def apply(f: (A => List[FieldValue]) => List[FieldValue]): Completable
}

final class InsertImpl[A, B <: Fields](
    contents: List[Fragment],
    model: Model[A, B]
) extends Insert[A, B] {

  def apply(
      f: (A => List[FieldValue]) => List[FieldValue]
  ): Completable =
    val names: List[Fragment]  = f(model.meta.mapper).map(as => as._1.name)
    val values: List[Fragment] = f(model.meta.mapper).map(as => as._2)

    val foldedFields = sql"(" |+| names
      .drop(1)
      .fold(names.head)((x, y) => x combine (sql", " |+| y)) |+| sql") "

    val foldedValues =
      sql"(" |+| values
        .drop(1)
        .fold(values.head)((x, y) => x combine (sql", " |+| y)) |+| sql") "

    new Completable(
      contents ++
        List(foldedFields, sql"values ", foldedValues)
    ) {}
}

object Insert {
  def apply[A, B <: Fields](
      contents: List[Fragment],
      model: Model[A, B]
  ) =
    new InsertImpl(contents, model)

}
