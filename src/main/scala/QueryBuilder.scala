import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._

trait QueryBuilder[A, B <: Fields]:
  def select: Where[A, B] & Completable

  def delete: Where[A, B]

  def insert: Insert[A, B]

  def update: Update[A, B]

final class QueryBuilderImpl[A, B <: Fields](
    model: Model[A, B]
) extends QueryBuilder[A, B]:

  val table = model.meta.table

  def select: Where[A, B] & Completable =
    Where(Query(Commands.select, table, List[Argument]()), model)

  def delete: Where[A, B] =
    Where(Query(Commands.delete, table, List[Argument]()), model)

  def insert: Insert[A, B] =
    Insert(Query(Commands.insert, table, List[Argument]()), model)

  def update: Update[A, B] =
    Update(Query(Commands.update, table, List[Argument]()), model)

object QueryBuilder:
  def apply[A, B <: Fields](model: Model[A, B]): QueryBuilder[A, B] =
    new QueryBuilderImpl(
      model = model
    )
