import Common._

import doobie.implicits._

final class QueryBuilder[A, B <: Model[A], C <: ModelMeta[A]](
    model: B,
    meta: C
) {

  def select: Where[A, B, C] =
    Where(List(sql"select * from ", sql"${meta.table} "), model, meta)

  def delete: NonCompletableWhere[A, B, C] =
    Where(List(sql"delete from ", sql"${meta.table} "), model, meta)

  def insert: Insert[A, B, C] =
    Insert(List(sql"insert into ", sql"${meta.table} "), model, meta)

  def update: Update[A, B, C] =
    Update(List(sql"update ", sql"${meta.table} "), model, meta)

}

object QueryBuilder {
  def apply[A, B <: Model[A], C <: ModelMeta[A]](model: B, meta: C) =
    new QueryBuilder(
      model,
      meta
    )
}
