package org.querypal.arguments

import org.querypal.logic.FragmentOperations.*
import org.querypal.logic.Model.*
import doobie.implicits.*
import cats.Monoid
import cats.implicits.*
import doobie.util.fragment.Fragment

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import org.querypal.logic.{Completable, Query, QueryType}
import org.querypal.logic.Completable.*

/** Insert receives an instance of A, and maps it to an insert values statement
  */
final class Insert[A, T <: QueryType](query: Query[T])(using meta: ModelMeta[A])
    extends (A => Completable[T]) { self =>
  def apply(entity: A): Completable[T] =
    val foldedFields =
      SqlOperations.commaSeparatedParened(meta.map(entity)._1.toList)

    val foldedValues =
      SqlOperations.commaSeparatedParened(meta.map(entity)._2.toList)

    new Completable[T]:
      val query: Query[T] =
        self.query.copy(arguments =
          self.query.arguments :+
            foldedFields :+
            Arguments.values :+
            foldedValues)

      def complete: Argument = query.complete

      def construct: Fragment = query.construct

      def constructString: String = query.constructString

      def executeOps[A](using TP: TypeProvider[T, A]): TP.R = TP.apply(query)

}
