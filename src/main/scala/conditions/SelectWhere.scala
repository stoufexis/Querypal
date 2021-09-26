import Common._
import doobie.util.fragment.Fragment
import doobie.implicits._
import FragmentOperations._
import FragmentOperations.Arguments

/** Select and where are intermediary classes. They help preserve the SQL-like
  * syntax of the querypal dsl. A is the entity being queried and B the object
  * containing A's modeled fields.
  */

final class Where[A, B <: Model[A]](model: B)(query: Query)
    extends Completable(query):

  def apply(all: "*"): Completable = new Completable(query) {}

  def apply(f: B => Condition): Conditional[A, B] & Completable =
    Conditional(model)(
      query.copy(arguments =
        query.arguments ++ List(Arguments.where) :+ f(model)
      )
    )

final class Select[A, B <: Model[A]](model: B)(query: Query) {
  def select: Where[A, B] = Where(model)(query.copy())
}

object Where:
  def apply[A, B <: Model[A]](model: B)(query: Query) =
    new Where[A, B](model)(query)

object Select:
  def apply[A, B <: Model[A]](model: B)(query: Query) =
    new Select[A, B](model)(query)
