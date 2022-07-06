package org.querypal.command

import org.querypal.logic.Model.*
import doobie.implicits.*
import doobie.util.fragment.Fragment
import org.querypal.logic.FragmentOperations.Commands
import org.querypal.logic.FragmentOperations.*
import org.querypal.conditions.*
import org.querypal.arguments.*
import org.querypal.logic.Join.*
import org.querypal.logic.{Query, QueryType}
import org.querypal.logic.Relation.*

/** The starting point of every query with A being the entity and B being the Model of A (an object
  * containing the fields).
  *
  * A subclass of Model[A] is required to preserve autocompletion.
  */
final class QueryBuilder[A, B <: Model[A]](model: B)(
    using meta: ModelMeta[A]):

  val table = meta.table

  def select: Where[A, B, QueryType.Select.type] & Joinable[A, B, QueryType.Select.type] =
    new WhereImpl(model)(Query(command = Commands.select, table = table))

  def delete: Where[A, B, QueryType.Delete.type] =
    new WhereImpl(model)(Query(command = Commands.delete, table = table))

  def insert: Insert[A, QueryType.Insert.type] =
    new Insert(Query(command = Commands.insert, table = table))

  def update(f: B => SetArgument): Set[A, B, QueryType.Update.type] =
    new Set(model)(
      Query(
        command = Commands.update,
        table = table,
        arguments = List(Arguments.set, f(model))))

object QueryBuilder:
  def apply[A: ModelMeta, B <: Model[A]](model: B): QueryBuilder[A, B] =
    new QueryBuilder(model)
