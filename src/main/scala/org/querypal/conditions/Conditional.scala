package org.querypal.conditions

import org.querypal.logic.Model.*
import doobie.util.fragment.Fragment
import doobie.implicits.*
import org.querypal.logic.FragmentOperations.*
import org.querypal.logic.{Completable, ConditionList, Query, QueryType}
import ConditionalHelpers.*
import org.querypal.logic.Join.{BiRelation, Joinable, JoinedJoinable, joinedSelect}
import org.querypal.logic.Completable.*

/** Conditional helps you construct the conditions of your query. With A being the entity queried
  * and B being the object containing the modeled fields of A. On each step, B is provided to take
  * advantage of autocompletes and create typed conditionals.
  */

final class Conditional[A, B <: Model[A], T <: QueryType](model: B)(query: Query[T])
    extends Joinable[A, B, T], Completable[T]:

  type BiRelationOfA[B] = BiRelation[A, B]

  def join[C: ModelMeta: BiRelationOfA, D <: Model[C]](toJoin: D): JoinedSelect[A, C, D, T] =
    joinedSelect(query, toJoin)

  private def nextConditional(q: Query[T]): Conditional[A, B, T] =
    new Conditional(model)(q)

  def and(f: B => Condition): Conditional[A, B, T] =
    nextConditional(andQuery(model, query, f))

  def or(f: B => Condition): Conditional[A, B, T] =
    nextConditional(orQuery(model, query, f))

  def bind(f: Conditional[A, B, T] => Conditional[A, B, T]): Conditional[A, B, T] =
    nextConditional(boundQuery(query, f, model))

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString

  def executeOps[A](using TP: TypeProvider[T, A]): TP.R = TP.apply(query)

final class JoinedConditional[A, B, C <: Model[B], T <: QueryType](model: C)(query: Query[T])
    extends JoinedJoinable[A, B, C, T], Completable[T]:

  type BiRelationOfA[B] = BiRelation[A, B]

  def join[C: ModelMeta: BiRelationOfA, D <: Model[C]](toJoin: D): JoinedSelect[A, C, D, T] =
    joinedSelect(query, toJoin)

  private def nextConditional(query: Query[T]): JoinedConditional[A, B, C, T] =
    new JoinedConditional(model)(query)

  def and(f: C => Condition): JoinedConditional[A, B, C, T] =
    nextConditional(andQuery(model, query, f))

  def or(f: C => Condition): JoinedConditional[A, B, C, T] =
    nextConditional(orQuery(model, query, f))

  def bind(f: Conditional[B, C, T] => Conditional[B, C, T]): JoinedConditional[A, B, C, T] =
    nextConditional(boundQuery(query, f, model))

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString

  def executeOps[A](using TP: TypeProvider[T, A]): TP.R = TP.apply(query)

object ConditionalHelpers:
  def boundQuery[A, B <: Model[A], T <: QueryType](
      query: Query[T],
      f: Conditional[A, B, T] => Conditional[A, B, T],
      model: B): Query[T] =
    query.copy(conditionList =
      query.conditionList
        .dropRightFromLast(1)
        .addToLast(
          GeneralOperators.leftParen,
          f(
            Conditional(model)(query =
              query.copy(conditionList =
                ConditionList(query.conditionList.last.last))))
            .complete,
          GeneralOperators.rightParen))

  def orQuery[A, B <: Model[A], T <: QueryType](model: B, query: Query[T], f: B => Condition): Query[T] =
    query.copy(conditionList =
      query.conditionList.addToLast(ConditionOperators.or, f(model)))

  def andQuery[A, B <: Model[A], T <: QueryType](model: B, query: Query[T], f: B => Condition): Query[T] =
    query.copy(conditionList =
      query.conditionList.addToLast(ConditionOperators.and, f(model)))
