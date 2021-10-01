package org.querypal.logic

import cats.implicits._
import Model._
import Relation._
import org.querypal.conditions.JoinedSelect
import FragmentOperations.{SqlOperations, Completable, Argument, Arguments}

object Join:
  def joinedSelect[A, B: ModelMeta, C <: Model[B]](query: Query, toJoin: C)(
      using Relation[A, B] | Relation[B, A]
  ): JoinedSelect[A, B, C] = JoinedSelect[A, B, C](toJoin)(
    query.copy(
      joins = query.joins :+ joinOp[A, B],
      conditionList = query.conditionList.addList
    )
  )

  def joinOp[A, B](using
      relation: Relation[A, B] | Relation[B, A],
      toMeta: ModelMeta[B]
  ): Argument = Arguments.innerJoin
    |+| toMeta.table.name
    |+| Arguments.on
    |+| relation.joinCondition

  type BiRelation[A, B] = Relation[A, B] | Relation[B, A]

  trait Joinable[A, B <: Model[A]]:
    def join[C: ModelMeta, D <: Model[C]](
        toJoin: D
    )(using BiRelation[A, C]): JoinedSelect[A, C, D]

  trait JoinedJoinable[A, B, C <: Model[B]]:
    def join[D: ModelMeta, E <: Model[D]](
        toJoin: E
    )(using BiRelation[A, D]): JoinedSelect[A, D, E]

  trait JoinableCompletable[A, B <: Model[A]]
      extends Joinable[A, B],
        Completable

  trait JoinedJoinableCompletable[A, B, C <: Model[B]]
      extends JoinedJoinable[A, B, C],
        Completable
