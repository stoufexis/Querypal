import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment

trait QueryBuilder[A, B <: Fields] {
  def select: Where[A, B] & Completable

  def delete: Where[A, B]

  def insert: Insert[A, B]

  def update: Update[A, B]
}

final class QueryBuilderImpl[A, B <: Fields](
    model: Model[A, B]
) extends QueryBuilder[A, B] {

  val tableName = model.meta.table

  def select: Where[A, B] & Completable =
    Where(
      List(sql"select * from ", sql"${tableName} "),
      model
    )

  def delete: Where[A, B] =
    Where(List(sql"delete from ", sql"${tableName} "), model)

  def insert: Insert[A, B] =
    Insert(List(sql"insert into ", sql"${tableName} "), model)

  def update: Update[A, B] =
    Update(List(sql"update ", sql"${tableName} "), model)

}

object QueryBuilder {
  def apply[A, B <: Fields](model: Model[A, B]): QueryBuilder[A, B] =
    new QueryBuilderImpl(
      model = model
    )
}
