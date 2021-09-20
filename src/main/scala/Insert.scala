import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment

final class Insert[A, B <: Model[A], C <: ModelMeta[A]](
    contents: List[Fragment],
    model: B,
    meta: C
) {

  def apply(
      f: (A => List[FieldValue]) => List[FieldValue]
  ): Completable[A] =
    val names: List[Fragment]  = f(meta.mapper).map(as => as._1.name)
    val values: List[Fragment] = f(meta.mapper).map(as => as._2)

    val foldedFields = sql"(" |+| names
      .drop(1)
      .fold(names.head)((x, y) => x combine (sql", " |+| y)) |+| sql") "

    val foldedValues =
      sql"(" |+| values
        .drop(1)
        .fold(values.head)((x, y) => x combine (sql", " |+| y)) |+| sql") "

    new Completable[A](
      contents ++
        List(foldedFields, sql"values ", foldedValues)
    ) {}
}

object Insert {
  def apply[A, B <: Model[A], C <: ModelMeta[A]](
      contents: List[Fragment],
      model: B,
      meta: C
  ) =
    new Insert(contents, model, meta)

}
