import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

trait Conditional[A, B <: Fields]:
  def and(f: B => Condition): Conditional[A, B] & Completable

  def or(f: B => Condition): Conditional[A, B] & Completable

  def bind(
      f: Conditional[A, B] => Conditional[A, B] & Completable
  ): Conditional[A, B] & Completable

final class ConditionalImpl[A, B <: Fields](
    query: Query,
    model: Model[A, B]
) extends Conditional[A, B]
    with Completable(query):

  def and(f: B => Condition): Conditional[A, B] & Completable =
    Conditional(
      query.copy(arguments =
        query.arguments ++ List(ConditionOperators.and) :+ f(model.fields)
      ),
      model
    )

  def or(f: B => Condition): Conditional[A, B] & Completable =
    Conditional(
      query.copy(arguments =
        query.arguments ++ List(ConditionOperators.or) :+ f(model.fields)
      ),
      model
    )

  def bind(
      f: Conditional[A, B] => Conditional[A, B] & Completable
  ): Conditional[A, B] & Completable =
    Conditional(
      query.copy(arguments =
        query.arguments
          .dropRight(1)
          :+ GeneralOperators.leftParen
          :+ f(Conditional(query.copy(arguments = List()), model)).complete
          :+ GeneralOperators.rightParen
      ),
      model
    )

object Conditional:
  def apply[A, B <: Fields](
      query: Query,
      model: Model[A, B]
  ) = new ConditionalImpl(
    query,
    model
  )
