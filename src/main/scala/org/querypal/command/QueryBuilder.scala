package org.querypal.command

import org.querypal.logic.Model._
import doobie.implicits._
import doobie.util.fragment.Fragment
import org.querypal.logic.FragmentOperations.Commands
import org.querypal.logic.FragmentOperations._
import org.querypal.conditions._
import org.querypal.arguments._
import org.querypal.logic.Join._
import org.querypal.logic.Query
import org.querypal.logic.Relation._

/** The starting point of every query with A being the entity and B being the
  * Model of A (an object containing the fields).
  *
  * A subclass of Model[A] is required to preserve autocompletion.
  */
final class QueryBuilder[A, B <: Model[A]](model: B)(using
    meta: ModelMeta[A]
):
  val table = meta.table

  def select: Where[A, B] & Joinable[A, B] = new WhereImpl[A, B](model)(
    Query(command = Commands.select, table = table)
  )

  def delete: Where[A, B] = new WhereImpl[A, B](model)(
    Query(command = Commands.delete, table = table)
  )

  def insert: Insert[A] = new Insert[A](
    Query(command = Commands.insert, table = table)
  )

  def update(f: B => SetArgument): Set[A, B] = new Set[A, B](model)(
    Query(
      command = Commands.update,
      table = table,
      arguments = List(Arguments.set, f(model))
    )
  )

object QueryBuilder:
  def apply[A: ModelMeta, B <: Model[A]](model: B): QueryBuilder[A, B] =
    new QueryBuilder(model)
