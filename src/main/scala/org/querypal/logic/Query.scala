package org.querypal.logic

import FragmentOperations.Argument.*
import FragmentOperations.*
import doobie.free.ConnectionIO
import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import doobie.util.update.Update0
import org.querypal.command.QueryBuilder
import org.querypal.logic.Model.*
import fs2.Stream

import scala.compiletime.erasedValue

enum QueryType:
  case Select
  case Insert
  case Update
  case Delete

/** The query in its preconstructed form
  */
case class Query[+T <: QueryType](
    command: Command,
    table: Table,
    arguments: List[Argument] = List(),
    conditionList: ConditionList = ConditionList(),
    joins: List[Argument] = List())


