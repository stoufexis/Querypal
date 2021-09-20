import Common._

import doobie.implicits._

final class QueryBuilder[A, B <: Fields](
    model: Model[A, B]
) {

  def select: Where[A, B] =
    Where(List(sql"select * from ", sql"${model.meta.table} "), model)

  def delete: NonCompletableWhere[A, B] =
    Where(List(sql"delete from ", sql"${model.meta.table} "), model)

  def insert: Insert[A, B] =
    Insert(List(sql"insert into ", sql"${model.meta.table} "), model)

  def update: Update[A, B] =
    Update(List(sql"update ", sql"${model.meta.table} "), model)

}

object QueryBuilder {
  def apply[A, B <: Fields](model: Model[A, B]) =
    new QueryBuilder(
      model
    )
}
