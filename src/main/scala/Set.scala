import doobie.util.fragment.Fragment
import doobie.implicits._
import Common._
import cats.implicits._
import FragmentOperations._
import FragmentOperations.SqlOperations._

final class Set[A, B <: Model[A]](model: B)(query: Query):
  def update(f: B => SetArgument) = new Set(model)(
    query.copy(arguments =
      query.arguments ++ List(GeneralOperators.comma, f(model))
    )
  )

  def where = Where(model)(query.copy())

object Set:
  def apply[A, B <: Model[A]](model: B)(query: Query) =
    new Set[A, B](model)(query)
