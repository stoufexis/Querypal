import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

/** Conditional helps you construct the conditions of your query. With A being
  * the entity queried and B being the object containing the modeled fields of
  * A. On each step, B is provided to take advantage of autocompletes and create
  * typed conditionals.
  */

final class Conditional[A, B <: Model[A]](model: B)(query: Query)
    extends Joinable[A, B],
      Completable:

  def join[C: ModelMeta: BiRelation, D <: Model[C]](
      toJoin: D
  ): JoinedSelect[A, C, D] =
    JoinedSelect(toJoin)(
      query.copy(joins = query.joins :+ SqlOperations.joinOp[A, C])
    )

  def and(f: B => Condition) =
    Conditional(model)(
      query.copy(arguments =
        query.arguments ++ List(ConditionOperators.and) :+ f(model)
      )
    )

  def or(f: B => Condition) =
    Conditional(model)(
      query.copy(arguments =
        query.arguments ++ List(ConditionOperators.or) :+ f(model)
      )
    )

  def bind(f: Conditional[A, B] => Conditional[A, B]) =
    Conditional(model)(
      query.copy(arguments =
        query.arguments
          .dropRight(1)
          :+ GeneralOperators.leftParen
          :+ f(
            Conditional(model)(
              query.copy(arguments = List(query.arguments.last))
            )
          ).complete
          :+ GeneralOperators.rightParen
      )
    )

  def complete: Argument = SqlOperations.complete(query)

  def construct: Fragment = SqlOperations.construct(query)

final class JoinedConditional[A, B, C <: Model[B]](model: C)(query: Query)
    extends JoinedJoinable[A, B, C],
      Completable:

  def join[C: ModelMeta: BiRelation, D <: Model[C]](
      toJoin: D
  ): JoinedSelect[A, C, D] =
    JoinedSelect(toJoin)(
      query.copy(joins = query.joins :+ SqlOperations.joinOp[A, C])
    )

  def and(f: C => Condition) =
    JoinedConditional(model)(
      query.copy(arguments =
        query.arguments ++ List(ConditionOperators.and) :+ f(model)
      )
    )

  def or(f: C => Condition) =
    Conditional(model)(
      query.copy(arguments =
        query.arguments ++ List(ConditionOperators.or) :+ f(model)
      )
    )

  def bind(
      f: Conditional[B, C] => Conditional[B, C]
  ): JoinedConditional[A, B, C] =
    JoinedConditional(model)(
      query.copy(arguments =
        query.arguments
          .dropRight(1)
          :+ GeneralOperators.leftParen
          :+ f(
            Conditional(model)(
              query.copy(arguments = List(query.arguments.last))
            )
          ).complete
          :+ GeneralOperators.rightParen
      )
    )

  def complete: Argument = SqlOperations.complete(query)

  def construct: Fragment = SqlOperations.construct(query)

object Conditional:
  def apply[A, B <: Model[A]](model: B)(query: Query): Conditional[A, B] =
    new Conditional[A, B](model)(query)

object JoinedConditional:
  def apply[A, B, C <: Model[B]](model: C)(
      query: Query
  ): JoinedConditional[A, B, C] =
    new JoinedConditional[A, B, C](model)(query)
