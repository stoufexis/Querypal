package org.querypal.arguments

import doobie.util.fragment.Fragment
import doobie.implicits._
import org.querypal.logic.Model._
import cats.implicits._
import org.querypal.logic.FragmentOperations._
import org.querypal.logic.FragmentOperations.SqlOperations._
import org.querypal.conditions.WhereImpl
import org.querypal.logic.Query

/** Set lets you construct an update command step by step, providing you with B
  * to get autocompletion and type safe set arguments
  */
final class Set[A, B <: Model[A]](model: B)(query: Query):
  def update(f: B => SetArgument) = new Set(model)(
    query.copy(arguments =
      query.arguments ++ List(GeneralOperators.comma, f(model))
    )
  )

  def where = new WhereImpl[A, B](model)(query)
