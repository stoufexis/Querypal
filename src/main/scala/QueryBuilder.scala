import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._

final class QueryBuilder[A, B <: Fields[A]](model: Model[A, B])(using
    meta: ModelMeta[A]
):
  val table = meta.table

  def select: WhereInterm[A, B] =
    new Interm(model)

  def delete: WhereInterm[A, B] =
    new Interm(model)

  def insert =
    Insert(model)(Query(Commands.insert, table, List[Argument]()))

  def update: SetInterm[A, B] =
    new Interm(model)

trait WhereInterm[A, B <: Fields[A]] {
  def where: Where[A, B]
}

trait SetInterm[A, B <: Fields[A]] {
  def set(f: B => EqualsCondition): Set[A, B]
}

final class Interm[A, B <: Fields[A]](model: Model[A, B])(using
    meta: ModelMeta[A]
) extends WhereInterm[A, B],
      SetInterm[A, B]:

  val table = meta.table

  def where =
    Where(model)(Query(Commands.select, table, List[Argument]()))

  def set(f: B => EqualsCondition) =
    Set(model)(
      Query(Commands.update, table, List(SqlOperations.set, f(model.fields)))
    )

object QueryBuilder:
  def apply[A: ModelMeta, B <: Fields[A]](
      model: Model[A, B]
  ): QueryBuilder[A, B] = new QueryBuilder(model)
