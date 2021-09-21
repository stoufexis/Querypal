import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._

trait QueryBuilder[A, B <: Fields]:
  def select: WhereInterm[A, B]

  def delete: WhereInterm[A, B]

  def insert: Insert[A, B]

  def update: SetInterm[A, B]

final class QueryBuilderImpl[A, B <: Fields](
    model: Model[A, B]
) extends QueryBuilder[A, B]:

  val table = model.meta.table

  def select: WhereInterm[A, B] =
    Interm(model)

  def delete: WhereInterm[A, B] =
    Interm(model)

  def insert: Insert[A, B] =
    Insert(Query(Commands.insert, table, List[Argument]()), model)

  def update: SetInterm[A, B] =
    Interm(model)

trait WhereInterm[A, B <: Fields] {
  def where: Where[A, B] & Completable
}

trait SetInterm[A, B <: Fields] {
  def set(f: B => EqualsCondition): Set[A, B]
}

final class Interm[A, B <: Fields](
    model: Model[A, B]
) extends WhereInterm[A, B]
    with SetInterm[A, B] {

  val table = model.meta.table

  def where: Where[A, B] & Completable =
    Where(Query(Commands.select, table, List[Argument]()), model)

  def set(f: B => EqualsCondition) =
    new Set(
      Query(
        Commands.update,
        table,
        List(SqlOperations.set, f(model.fields))
      ),
      model
    )

}

object QueryBuilder:
  def apply[A, B <: Fields](model: Model[A, B]): QueryBuilder[A, B] =
    new QueryBuilderImpl(
      model = model
    )
