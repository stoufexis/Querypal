import FragmentOperations._
import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

final class Insert[A <: Product, B <: Model[A]](model: B)(query: Query)(using
    meta: ModelMeta[A]
):
  def apply(entity: A): Completable =
    val foldedFields =
      SqlOperations.commaSeparatedParened(
        meta.map(entity)._1.toList
      )

    val foldedValues =
      SqlOperations.commaSeparatedParened(
        meta.map(entity)._2.toList
      )

    new Completable(
      query.copy(arguments =
        query.arguments ++ List(foldedFields, Arguments.values, foldedValues)
      )
    ) {}

object Insert:
  def apply[A <: Product: ModelMeta, B <: Model[A]](model: B)(query: Query) =
    new Insert(model)(query)
