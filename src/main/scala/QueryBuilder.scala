import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._
import javax.management.relation.Relation

final class QueryBuilder[A, B](model: Model[A, B])(using meta: ModelMeta[A]):
  val table = meta.table

  def select: WhereInterm[A, B] =
    new Interm(Query(Commands.select, table, List[Argument]()), model)

  def join[C: ModelMeta](using
      Relationship[A, C] | Relationship[C, A]
  ): WhereInterm[A, B] =
    new Interm(
      Query(Commands.select, table, List[Argument](SqlOperations.joinOp[A, C])),
      model
    )

  def delete: WhereInterm[A, B] =
    new Interm(Query(Commands.delete, table, List[Argument]()), model)

  def insert =
    Insert(model)(Query(Commands.insert, table, List[Argument]()))

  def update: SetInterm[A, B] =
    new Interm(Query(Commands.update, table, List[Argument]()), model)

trait WhereInterm[A, B]:
  def where: CompletableWhere[A, B]

trait SetInterm[A, B]:
  def set(f: B => EqualsCondition): Set[A, B]

final class Interm[A, B](query: Query, model: Model[A, B])(using
    meta: ModelMeta[A]
) extends WhereInterm[A, B],
      SetInterm[A, B]:

  val table = meta.table

  def where = Where(model)(query)

  def set(f: B => EqualsCondition) = Set(model)(
    query.copy(arguments =
      query.arguments ++ List(SqlOperations.set, f(model.fields))
    )
  )

object QueryBuilder:
  def apply[A: ModelMeta, B](model: Model[A, B]): QueryBuilder[A, B] =
    new QueryBuilder(model)
