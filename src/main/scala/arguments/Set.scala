package arguments

import doobie.util.fragment.Fragment
import doobie.implicits._
import logic.Common._
import cats.implicits._
import logic.FragmentOperations._
import logic.FragmentOperations.SqlOperations._
import conditions.Where

/** Set lets you construct an update command step by step, providing you with B
  * to get autocompletion and type safe set arguments
  */

final class Set[A, B <: Model[A]](model: B)(query: Query):
  def update(f: B => SetArgument) = new Set(model)(
    query.copy(arguments =
      query.arguments ++ List(GeneralOperators.comma, f(model))
    )
  )

  def where = Where(model)(query)

object Set:
  def apply[A, B <: Model[A]](model: B)(query: Query) =
    new Set[A, B](model)(query)
