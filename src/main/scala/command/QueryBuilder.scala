import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._
import javax.management.relation.Relation

final class QueryBuilder[A <: Product, B <: Model[A]](model: B)(using
    meta: ModelMeta[A]
):
  val table = meta.table

  def select: Where[A, B] = new Select(model)(
    Query(Commands.select, table, List[Argument]())
  ).select

  type BiRef[B] = Ref[A, B] | Ref[B, A]

  def join[C: ModelMeta: BiRef]: Select[A, B] =
    Select(model)(
      Query(Commands.select, table, List[Argument](SqlOperations.joinOp[A, C]))
    )

  def delete: Where[A, B] = new Select(model)(
    Query(Commands.delete, table, List[Argument]())
  ).select

  def insert: Insert[A, B] =
    Insert(model)(Query(Commands.insert, table, List[Argument]()))

  def update(f: B => SetArgument): Set[A, B] =
    Set(model)(
      Query(Commands.update, table, List(Arguments.set, f(model)))
    )

object QueryBuilder:
  def apply[A <: Product: ModelMeta, B <: Model[A]](
      model: B
  ): QueryBuilder[A, B] =
    new QueryBuilder(model)
