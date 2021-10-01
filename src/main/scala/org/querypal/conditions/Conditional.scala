package org.querypal.conditions

import org.querypal.logic.Model._
import doobie.util.fragment.Fragment
import doobie.implicits._
import org.querypal.logic.FragmentOperations._
import org.querypal.logic.ConditionList
import ConditionalHelpers._
import org.querypal.logic.Query
import org.querypal.logic.Join.{
  Joinable,
  joinedSelect,
  JoinedJoinable,
  BiRelation
}

/** Conditional helps you construct the conditions of your query. With A being
  * the entity queried and B being the object containing the modeled fields of
  * A. On each step, B is provided to take advantage of autocompletes and create
  * typed conditionals.
  */

final class Conditional[A, B <: Model[A]](model: B)(query: Query)
    extends Joinable[A, B],
      Completable:

  type BiRelationOfA[B] = BiRelation[A, B]

  def join[C: ModelMeta: BiRelationOfA, D <: Model[C]](
      toJoin: D
  ): JoinedSelect[A, C, D] =
    joinedSelect[A, C, D](query, toJoin)

  private def nextConditional(query: Query): Conditional[A, B] =
    new Conditional[A, B](model)(query)

  def and(f: B => Condition): Conditional[A, B] =
    nextConditional(andQuery(model, query, f))

  def or(f: B => Condition): Conditional[A, B] =
    nextConditional(orQuery(model, query, f))

  def bind(f: Conditional[A, B] => Conditional[A, B]): Conditional[A, B] =
    nextConditional(boundQuery(query, f, model))

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString

final class JoinedConditional[A, B, C <: Model[B]](model: C)(query: Query)
    extends JoinedJoinable[A, B, C],
      Completable:

  type BiRelationOfA[B] = BiRelation[A, B]

  def join[C: ModelMeta: BiRelationOfA, D <: Model[C]](
      toJoin: D
  ): JoinedSelect[A, C, D] = joinedSelect[A, C, D](query, toJoin)

  private def nextConditional(query: Query): JoinedConditional[A, B, C] =
    new JoinedConditional[A, B, C](model)(query)

  def and(f: C => Condition): JoinedConditional[A, B, C] =
    nextConditional(andQuery(model, query, f))

  def or(f: C => Condition): JoinedConditional[A, B, C] =
    nextConditional(orQuery(model, query, f))

  def bind(
      f: Conditional[B, C] => Conditional[B, C]
  ): JoinedConditional[A, B, C] =
    nextConditional(boundQuery(query, f, model))

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString

object ConditionalHelpers:
  def boundQuery[A, B <: Model[A]](
      query: Query,
      f: Conditional[A, B] => Conditional[A, B],
      model: B
  ) =
    query.copy(conditionList =
      query.conditionList
        .dropRightFromLast(1)
        .addToLast(
          GeneralOperators.leftParen,
          f(
            Conditional(model)(
              query = query
                .copy(conditionList =
                  ConditionList(query.conditionList.last.last)
                )
            )
          ).complete,
          GeneralOperators.rightParen
        )
    )

  def orQuery[A, B <: Model[A]](model: B, query: Query, f: B => Condition) =
    query.copy(conditionList =
      query.conditionList.addToLast(ConditionOperators.or, f(model))
    )

  def andQuery[A, B <: Model[A]](model: B, query: Query, f: B => Condition) =
    query.copy(conditionList =
      query.conditionList.addToLast(ConditionOperators.and, f(model))
    )
