package org.querypal.conditions

import org.querypal.logic.FragmentOperations._
import doobie.util.fragment.Fragment
import org.querypal.logic.Model._
import org.querypal.logic.Join.{
  Joinable,
  JoinedJoinable,
  BiRelation,
  joinedSelect,
  JoinableCompletable,
  JoinedJoinableCompletable
}

import org.querypal.logic.Query

trait JoinedWhere[A, B, C <: Model[B]] extends Completable {
  def apply(all: "* "): JoinedJoinable[A, B, C] & Completable

  def apply(f: C => Condition): JoinedConditional[A, B, C] & Completable

  def complete: Argument

  def construct: Fragment
}

trait Where[A, B <: Model[A]] extends Completable {
  def apply(all: "* "): Joinable[A, B] & Completable

  def apply(f: B => Condition): Conditional[A, B] & Completable

  def complete: Argument

  def construct: Fragment
}

final class WhereImpl[A, B <: Model[A]](model: B)(query: Query)
    extends Where[A, B],
      Joinable[A, B] { self =>

  type ABiRelation[B] = BiRelation[A, B]
  def join[C: ModelMeta: ABiRelation, D <: Model[C]](
      toJoin: D
  ): JoinedSelect[A, C, D] =
    joinedSelect[A, C, D](query, toJoin)

  def apply(all: "* "): Joinable[A, B] & Completable =
    new JoinableCompletable[A, B]:
      val query = self.query

      type ABiRelation[B] = BiRelation[A, B]
      def join[C: ModelMeta: ABiRelation, D <: Model[C]](
          toJoin: D
      ): JoinedSelect[A, C, D] =
        joinedSelect[A, C, D](query, toJoin)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct

      def constructString: String = query.constructString

  def apply(f: B => Condition): Conditional[A, B] & Completable =
    Conditional[A, B](model)(
      query.copy(conditionList = query.conditionList.addToLast(f(model)))
    )

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString
}

final class JoinedWhereImpl[A, B, C <: Model[B]](model: C)(query: Query)
    extends JoinedWhere[A, B, C],
      JoinedJoinable[A, B, C] { self =>

  type ABiRelation[B] = BiRelation[A, B]
  def join[D: ModelMeta: ABiRelation, E <: Model[D]](
      toJoin: E
  ): JoinedSelect[A, D, E] =
    joinedSelect[A, D, E](query, toJoin)

  def apply(all: "* "): JoinedJoinable[A, B, C] & Completable =
    new JoinedJoinableCompletable[A, B, C]:
      val query =
        self.query.copy(arguments = self.query.arguments.dropRight(1))

      type ABiRelation[B] = BiRelation[A, B]

      def join[D: ModelMeta: ABiRelation, E <: Model[D]](
          toJoin: E
      ): JoinedSelect[A, D, E] =
        joinedSelect[A, D, E](query, toJoin)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct

      def constructString: String = query.constructString

  def apply(f: C => Condition): JoinedConditional[A, B, C] & Completable =
    JoinedConditional[A, B, C](model)(
      query.copy(conditionList = query.conditionList.addToLast(f(model)))
    )

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString
}
