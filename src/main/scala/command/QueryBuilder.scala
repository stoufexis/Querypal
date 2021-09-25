import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._
import FragmentOperations.Relation

final class QueryBuilder[A, B <: Model[A]](model: B)(using
    meta: ModelMeta[A]
):
  val table = meta.table

  def select: Where[A, B] = new Select(model)(
    Query(Commands.select, table, List[Argument]())
  ).select

  type BiRelation[B] = Relation[A, B] | Relation[B, A]

  def join[C: ModelMeta: BiRelation](
      toJoin: Model[C]
  ): Select[A, B] =
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

object QueryStart:
  def apply[A: ModelMeta, B <: Model[A]](
      model: B
  ): QueryBuilder[A, B] =
    new QueryBuilder(model)
