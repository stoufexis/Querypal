package org.querypal.conditions

import org.querypal.logic.FragmentOperations.*
import doobie.util.fragment.Fragment
import org.querypal.logic.Model.*
import org.querypal.logic.Join.{
  BiRelation,
  Joinable,
  JoinableCompletable,
  JoinedJoinable,
  JoinedJoinableCompletable,
  joinedSelect
}
import org.querypal.logic.Query
import org.querypal.logic.QueryType._

trait JoinedWhere[A, B, C <: Model[B], T <: QueryType] extends Completable[T]:
  def apply(all: "* "): JoinedJoinable[A, B, C, T] & Completable[T]

  def apply(f: C => Condition): JoinedConditional[A, B, C, T] & Completable[T]


trait Where[A, B <: Model[A], T <: QueryType] extends Completable[T]:
  def apply(all: "* "): Joinable[A, B, T] & Completable[T]

  def apply(f: B => Condition): Conditional[A, B, T] & Completable[T]

final class WhereImpl[A, B <: Model[A], T <: QueryType](model: B)(query: Query[T])
    extends Where[A, B, T], Joinable[A, B, T] { self =>

  type ABiRelation[B] = BiRelation[A, B]
  def join[C: ModelMeta: ABiRelation, D <: Model[C]](toJoin: D): JoinedSelect[A, C, D, T] =
    joinedSelect[A, C, D, T](query, toJoin)

  def apply(all: "* "): Joinable[A, B, T] & Completable[T] =
    new JoinableCompletable[A, B, T]:
      val query: Query[T] = self.query

      type ABiRelation[B] = BiRelation[A, B]
      def join[C: ModelMeta: ABiRelation, D <: Model[C]](toJoin: D): JoinedSelect[A, C, D, T] =
        joinedSelect[A, C, D, T](query, toJoin)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct

      def constructString: String = query.constructString

      def getQuery: Query[T] = query

  def apply(f: B => Condition): Conditional[A, B, T] & Completable[T] =
    Conditional(model)(
      query.copy(conditionList = query.conditionList.addToLast(f(model))))

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString

  def getQuery: Query[T] = query
}

final class JoinedWhereImpl[A, B, C <: Model[B], T <: QueryType](model: C)(query: Query[T])
    extends JoinedWhere[A, B, C, T], JoinedJoinable[A, B, C, T] { self =>

  type ABiRelation[B] = BiRelation[A, B]
  def join[D: ModelMeta: ABiRelation, E <: Model[D]](toJoin: E): JoinedSelect[A, D, E, T] =
    joinedSelect(query, toJoin)

  def apply(all: "* "): JoinedJoinable[A, B, C, T] & Completable[T] =
    new JoinedJoinableCompletable:
      val query: Query[T] =
        self.query.copy(arguments = self.query.arguments.dropRight(1))

      type ABiRelation[B] = BiRelation[A, B]

      def join[D: ModelMeta: ABiRelation, E <: Model[D]](toJoin: E): JoinedSelect[A, D, E, T] =
        joinedSelect(query, toJoin)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct

      def constructString: String = query.constructString

      def getQuery: Query[T] = query

  def apply(f: C => Condition): JoinedConditional[A, B, C, T] & Completable[T] =
    JoinedConditional(model)(
      query.copy(conditionList = query.conditionList.addToLast(f(model))))

  def complete: Argument = query.complete

  def construct: Fragment = query.construct

  def constructString: String = query.constructString

  def getQuery: Query[T] = self.query
}
