import Common._

import doobie.implicits._

final class QueryBuilder[A, B <: Model[A]](
    model: B,
    meta: ModelMeta[A]
) {

  def select: Where[A, B] =
    Where(List(sql"select * from ", sql"${meta.table} "), model, meta)

  def delete: NonCompletableWhere[A, B] =
    Where(List(sql"delete from ", sql"${meta.table} "), model, meta)

  def insert: Insert[A, B] =
    Insert(List(sql"insert into ", sql"${meta.table} "), model, meta)

  def update: Update[A, B] =
    Update(List(sql"update ", sql"${meta.table} "), model, meta)

}

object QueryBuilder {
  def apply[A, B <: Model[A]](model: B, meta: ModelMeta[A]) =
    new QueryBuilder(
      model,
      meta
    )
}
