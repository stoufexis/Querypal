import Common._

import doobie.implicits._

final class QueryBuilder[A, B <: Model[A]](model: B) {

  def select: Where[A, B] =
    Where(List(sql"select * from ", sql"${model.table} "), model = model)

  def delete: NonCompletableWhere[A, B] =
    Where(List(sql"delete from ", sql"${model.table} "), model = model)

  def insert: Insert[A, B] =
    Insert(List(sql"insert into ", sql"${model.table} "), model = model)

  // def update: Update[A] =
  //   Update(sql"update ", sql"${model.table} ", model = model)

}

object QueryBuilder {
  def apply[A, B <: Model[A]](model: B) = new QueryBuilder(model)
}
