package org.querypal.conditions

import org.querypal.logic.Model.*
import org.querypal.logic.FragmentOperations.*
import org.querypal.logic.Join.*
import org.querypal.logic.{Query, QueryType}

final class Select[A, B <: Model[A], T <: QueryType](model: B)(query: Query[T]) {
  def select: Where[A, B, T] & Joinable[A, B, T] =
    new WhereImpl(model)(query)
}

final class JoinedSelect[A, B, C <: Model[B], T <: QueryType](model: C)(query: Query[T]) {
  def select: JoinedWhere[A, B, C, T] & JoinedJoinable[A, B, C, T] =
    new JoinedWhereImpl(model)(query)
}
