package org.querypal.arguments

import doobie.util.fragment.Fragment
import doobie.implicits.*
import org.querypal.logic.Model.*
import cats.implicits.*
import org.querypal.logic.FragmentOperations.*
import org.querypal.logic.FragmentOperations.SqlOperations.*
import org.querypal.conditions.WhereImpl
import org.querypal.logic.Query
import org.querypal.logic.QueryType

/** Set lets you construct an update command step by step, providing you with B to get
  * autocompletion and type safe set arguments
  */
final class Set[A, B <: Model[A], T <: QueryType](model: B)(query: Query[T]):
  def update(f: B => SetArgument) = new Set(model)(
    query.copy(
      arguments = query.arguments ++ List(GeneralOperators.comma, f(model))))

  def where = new WhereImpl[A, B, T](model)(query)
