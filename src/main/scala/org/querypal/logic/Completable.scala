package org.querypal.logic

import doobie.free.ConnectionIO
import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import doobie.util.update.Update0
import fs2.Stream
import org.querypal.logic.FragmentOperations.Argument
import org.querypal.logic.Model.ModelMeta
import org.querypal.logic.{Query, QueryType}
import Completable._

trait Completable[T <: QueryType]:
  def complete: Argument
  def construct: Fragment
  def constructString: String
  def executeOps[A](using T: TypeProvider[T, A]): T.R

object Completable:
  extension [T <: QueryType](query: Query[T])
    def complete: Argument = query.conditionList.conditions.flatten.foldArgs

    def construct: Fragment =
      Update0(
        query.constructString,
        None)
        .toFragment

    def constructString: String = (
      List(query.command, query.table.name)
        ++ query.joins
        ++ query.arguments
        :+ query.conditionList.fold)
      .foldArgs.toString.trim
      .replaceAll(" +", " ")

  trait SelectExecuteOps[A]:
    def query: Query0[A]

  trait UpdateExecuteOps[A]:
    def withUniqueGeneratedKeys: ConnectionIO[A]
    def withGeneratedKeys: Stream[ConnectionIO, A]

  trait TypeProvider[-T <: QueryType, A]:
    type R
    def apply(query: Query[T]): R

  given [A](using Read[A]): TypeProvider[QueryType.Select.type, A] with
    type R = SelectExecuteOps[A]

    def apply(q: Query[QueryType.Select.type]): R = new SelectExecuteOps:
      def query: Query0[A] = q.construct.query[A]

type UpdateQueryTypes =
  QueryType.Update.type | QueryType.Insert.type | QueryType.Delete.type

given [A](using Read[A])(using M: ModelMeta[A]): TypeProvider[UpdateQueryTypes, A] with
  type R = UpdateExecuteOps[A]

  def apply(q: Query[UpdateQueryTypes]): R =
    new UpdateExecuteOps:
      override def withUniqueGeneratedKeys: ConnectionIO[A] =
        q.construct.update.withUniqueGeneratedKeys[A](M.columnNames: _*)

      override def withGeneratedKeys: Stream[ConnectionIO, A] =
        q.construct.update.withGeneratedKeys[A](M.columnNames: _*)
