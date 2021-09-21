import doobie.util.fragment.Fragment
import doobie.implicits._
import Common._
import cats.implicits._
import FragmentOperations._
import FragmentOperations.SqlOperations._

final class Set[A, B <: Fields](query: Query, model: Model[A, B]):
  def set(f: B => EqualsCondition) = new Set(
    query.copy(arguments =
      query.arguments ++ List(GeneralOperators.comma, f(model.fields))
    ),
    model
  )

  def where: Where[A, B] = Where(query.copy(), model)

object Set:
  def apply[A, B <: Fields](
      query: Query,
      model: Model[A, B]
  ) = new Set(query, model)
