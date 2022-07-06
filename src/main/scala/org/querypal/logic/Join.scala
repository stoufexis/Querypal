package org.querypal.logic

import cats.implicits._
import Model._
import Relation._
import org.querypal.conditions.JoinedSelect
import FragmentOperations.{SqlOperations, Argument, Arguments}
import org.querypal.logic.QueryType._

object Join:
  def joinedSelect[A, B: ModelMeta, C <: Model[B], T <: QueryType](query: Query[T], toJoin: C)(
      using Relation[A, B] | Relation[B, A]
  ): JoinedSelect[A, B, C, T] = JoinedSelect(toJoin)(
    query.copy(
      joins = query.joins :+ joinOp[A, B],
      conditionList = query.conditionList.addList))

  def joinOp[A, B](using
      relation: Relation[A, B] | Relation[B, A],
      toMeta: ModelMeta[B]
  ): Argument =
    Arguments.innerJoin
      |+| toMeta.table.name
      |+| Arguments.on
      |+| relation.joinCondition

  type BiRelation[A, B] = Relation[A, B] | Relation[B, A]

  trait Joinable[A, B <: Model[A], T <: QueryType]:
    def join[C: ModelMeta, D <: Model[C]](
        toJoin: D)(
        using BiRelation[A, C]): JoinedSelect[A, C, D, T]

  trait JoinedJoinable[A, B, C <: Model[B], T <: QueryType]:
    def join[D: ModelMeta, E <: Model[D]](
        toJoin: E)(
        using BiRelation[A, D]): JoinedSelect[A, D, E, T]

  trait JoinableCompletable[A, B <: Model[A], T <: QueryType]
      extends Joinable[A, B, T],
        Completable[T]

  trait JoinedJoinableCompletable[A, B, C <: Model[B], T <: QueryType]
      extends JoinedJoinable[A, B, C, T],
        Completable[T]
