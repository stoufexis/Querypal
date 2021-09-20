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
    content: List[Fragment] = List(),
    model: Model[A, B]
) extends QueryBuilder[A, B] {

  def select: Where[A, B] & Completable =
    Where(
      content ++ List(sql"select * from ", sql"${model.meta.table} "),
      model
    )

  def delete: Where[A, B] =
    Where(content ++ List(sql"delete from ", sql"${model.meta.table} "), model)

  def insert: Insert[A, B] =
    Insert(content ++ List(sql"insert into ", sql"${model.meta.table} "), model)

  def update: Update[A, B] =
    Update(content ++ List(sql"update ", sql"${model.meta.table} "), model)

}

object QueryBuilder {
  def apply[A, B <: Fields](model: Model[A, B]): QueryBuilder[A, B] =
    new QueryBuilderImpl(
      model = model
    )
}
