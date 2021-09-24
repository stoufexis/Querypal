import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._
import javax.management.relation.Relation

final class QueryBuilder[A <: Product: Mapper, B <: Model[A]](model: B)(using
    meta: ModelMeta[A]
):
  val table = meta.table

  def select: Where[A, B] =
    Where(model)(Query(Commands.select, table, List[Argument]()))

  def join[C: ModelMeta](using
      Relationship[A, C] | Relationship[C, A]
  ): Select[A, B] =
    new Select(model)(
      Query(Commands.select, table, List[Argument](SqlOperations.joinOp[A, C]))
    )

  def delete: Where[A, B] =
    Where(model)(Query(Commands.delete, table, List[Argument]()))

  def insert =
    Insert(model)(Query(Commands.insert, table, List[Argument]()))

  def update(f: B => SetArgument): Set[A, B] =
    new Set(model)(
      Query(Commands.update, table, List(SqlOperations.set, f(model)))
    )

final class Select[A, B <: Model[A]](model: B)(query: Query) {
  def select: Where[A, B] = Where(model)(query.copy())
}

object QueryBuilder:
  def apply[A <: Product: ModelMeta: Mapper, B <: Model[A]](
      model: B
  ): QueryBuilder[A, B] =
    new QueryBuilder(model)
