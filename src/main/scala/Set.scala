import doobie.util.fragment.Fragment
import doobie.implicits._
import Common._
import cats.implicits._
import FragmentOperations._
import FragmentOperations.SqlOperations._

final class Set[A, B](model: Model[A, B])(query: Query):
  def set(f: B => EqualsCondition) = new Set(model)(
    query.copy(arguments =
      query.arguments ++ List(GeneralOperators.comma, f(model.fields))
    )
  )

  def where = Where(model)(query.copy())

object Set:
  def apply[A, B](model: Model[A, B])(query: Query) =
    new Set[A, B](model)(query)
