package conditions

import logic.Model._
import logic.FragmentOperations._
import logic.Join._
import logic.Query

final class Select[A, B <: Model[A]](model: B)(query: Query) {
  def select: Where[A, B] & Joinable[A, B] =
    new WhereImpl[A, B](model)(query)
}

final class JoinedSelect[A, B, C <: Model[B]](model: C)(query: Query) {
  def select: JoinedWhere[A, B, C] & JoinedJoinable[A, B, C] =
    new JoinedWhereImpl[A, B, C](model)(query)
}
