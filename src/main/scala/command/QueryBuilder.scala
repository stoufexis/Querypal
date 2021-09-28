import Common._

import doobie.implicits._
import doobie.util.fragment.Fragment
import FragmentOperations.Commands
import FragmentOperations._

/** The starting point of every query with A being the entity and B being the
  * Model of A (an object containing the fields).
  *
  * A subclass of Model[A] is required to preserve autocompletion.
  */

final class QueryBuilder[A, B <: Model[A]](model: B)(using
    meta: ModelMeta[A]
):
  val table = meta.table

  def select: Where[A, B] & Joinable[A, B] = Where(model)(
    Query(Commands.select, table, List(Arguments.where))
  )

  type BiRelation[B] = Relation[A, B] | Relation[B, A]

  def delete: Where[A, B] = Where(model)(
    Query(Commands.delete, table, List[Argument]())
  )

  def insert: Insert[A] =
    Insert(Query(Commands.insert, table, List[Argument]()))

  def update(f: B => SetArgument): Set[A, B] =
    Set(model)(
      Query(Commands.update, table, List(Arguments.set, f(model)))
    )

object QueryStart:
  def apply[A: ModelMeta, B <: Model[A]](model: B): QueryBuilder[A, B] =
    new QueryBuilder(model)
