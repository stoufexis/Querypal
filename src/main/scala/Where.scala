import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import FragmentOperations.Arguments

trait Where[A, B <: Fields] {
  def where(f: B => Condition): Conditional[A, B] & Completable
}

final class WhereImpl[A, B <: Fields](
    query: Query,
    model: Model[A, B]
) extends Where[A, B]
    with Completable(query):
  def where(f: B => Condition): Conditional[A, B] & Completable =
    Conditional(
      query.copy(arguments =
        query.arguments ++ List(Arguments.where) :+ f(model.fields)
      ),
      model
    )

object Where:
  def apply[A, B <: Fields](
      query: Query,
      model: Model[A, B]
  ) = new WhereImpl(
    query,
    model
  )
