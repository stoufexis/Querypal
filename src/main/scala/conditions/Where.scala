package conditions
import logic.FragmentOperations._
import doobie.util.fragment.Fragment
import logic.Model._
import logic.Join.{
  Joinable,
  JoinedJoinable,
  BiRelation,
  joinedSelect,
  JoinableCompletable,
  JoinedJoinableCompletable
}

import logic.Query

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

  def join[C: ModelMeta, D <: Model[C]](
      toJoin: D
  )(using BiRelation[A, C]): JoinedSelect[A, C, D] =
    joinedSelect[A, C, D](query, toJoin)

  def apply(all: "* "): Joinable[A, B] & Completable =
    new JoinableCompletable[A, B] {
      private val query = self.query

      def join[C: ModelMeta, D <: Model[C]](
          toJoin: D
      )(using BiRelation[A, C]): JoinedSelect[A, C, D] =
        joinedSelect[A, C, D](query, toJoin)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct
    }

  def apply(f: B => Condition): Conditional[A, B] & Completable =
    Conditional[A, B](model)(
      query.copy(conditions = query.conditions.addToLast(f(model)))
    )

  def complete: Argument = query.complete

  def construct: Fragment = query.construct
}

final class JoinedWhereImpl[A, B, C <: Model[B]](model: C)(query: Query)
    extends JoinedWhere[A, B, C],
      JoinedJoinable[A, B, C] { self =>

  def join[D: ModelMeta, E <: Model[D]](
      toJoin: E
  )(using BiRelation[A, D]): JoinedSelect[A, D, E] =
    joinedSelect[A, D, E](query, toJoin)

  def apply(all: "* "): JoinedJoinable[A, B, C] & Completable =
    new JoinedJoinableCompletable[A, B, C] {
      val query =
        self.query.copy(arguments = self.query.arguments.dropRight(1))

      def join[D: ModelMeta, E <: Model[D]](
          toJoin: E
      )(using BiRelation[A, D]): JoinedSelect[A, D, E] =
        joinedSelect[A, D, E](query, toJoin)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct
    }

  def apply(f: C => Condition): JoinedConditional[A, B, C] & Completable =
    JoinedConditional[A, B, C](model)(
      query.copy(conditions = query.conditions.addToLast(f(model)))
    )

  def complete: Argument = query.complete

  def construct: Fragment = query.construct
}
