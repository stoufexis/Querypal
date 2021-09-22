import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import FragmentOperations.Arguments

trait Where[A, B <: Fields[A]] {
  def apply(f: B => Condition): Conditional[A, B] & Completable
}

final class WhereImpl[A, B <: Fields[A]](model: Model[A, B])(query: Query)
    extends Where[A, B],
      Completable(query):

  def apply(f: B => Condition): Conditional[A, B] & Completable =
    Conditional(model)(
      query.copy(arguments =
        query.arguments ++ List(Arguments.where) :+ f(model.fields)
      )
    )

object Where:
  def apply[A, B <: Fields[A]](model: Model[A, B])(query: Query) =
    new WhereImpl(model)(query)
