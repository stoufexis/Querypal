import FragmentOperations._
import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment

final class Insert[A, B <: Fields[A]](model: Model[A, B])(query: Query)(using
    meta: ModelMeta[A]
):
  def apply(entity: A): Completable =
    val names: List[Fragment]  = meta.mapper(entity).map(as => as._1.name)
    val values: List[Fragment] = meta.mapper(entity).map(as => as._2)

    val foldedFields = SqlOperations.commaSeparatedParened(names)

    val foldedValues = SqlOperations.commaSeparatedParened(values)

    new Completable(
      query.copy(arguments =
        query.arguments ++ List(foldedFields, Arguments.values, foldedValues)
      )
    ) {}

object Insert:
  def apply[A: ModelMeta, B <: Fields[A]](model: Model[A, B])(query: Query) =
    new Insert(model)(query)
