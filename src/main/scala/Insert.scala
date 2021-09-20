import FragmentOperations._
import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment

trait Insert[A, B <: Fields] {
  def apply(f: (A => List[FieldValue]) => List[FieldValue]): Completable
}

final class InsertImpl[A, B <: Fields](
    query: Query,
    model: Model[A, B]
) extends Insert[A, B]:

  def apply(
      f: (A => List[FieldValue]) => List[FieldValue]
  ): Completable =
    val names: List[Fragment]  = f(model.meta.mapper).map(as => as._1.name)
    val values: List[Fragment] = f(model.meta.mapper).map(as => as._2)

    val foldedFields = SqlOperations.commaSeparatedParened(names)

    val foldedValues = SqlOperations.commaSeparatedParened(values)

    new Completable(
      query.copy(arguments =
        query.arguments ++ List(foldedFields, Arguments.values, foldedValues)
      )
    ) {}

object Insert:
  def apply[A, B <: Fields](
      query: Query,
      model: Model[A, B]
  ) =
    new InsertImpl(query, model)
