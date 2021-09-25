import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._

final class Conditional[A, B <: Model[A]](model: B)(
    query: Query
) extends Completable(query):

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

  def bind(
      f: Conditional[A, B] => Conditional[A, B]
  ) =
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

object Conditional:
  def apply[A, B <: Model[A]](model: B)(query: Query) =
    new Conditional[A, B](model)(query)
