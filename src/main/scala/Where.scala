import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import FragmentOperations.Arguments

trait Where[A, B <: Model[A]] {
  def apply(f: B => Condition): Conditional[A, B] & Completable
}

final class CompletableWhere[A, B <: Model[A]](model: B)(query: Query)
    extends Where[A, B],
      Completable(query):

  def apply(f: B => Condition): Conditional[A, B] & Completable =
    Conditional(model)(
      query.copy(arguments =
        query.arguments ++ List(Arguments.where) :+ f(model)
      )
    )

object Where:
  def apply[A, B <: Model[A]](model: B)(query: Query) =
    new CompletableWhere[A, B](model)(query)
