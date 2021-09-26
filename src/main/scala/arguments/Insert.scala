import FragmentOperations._
import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import FragmentOperations.Completable

/** Insert receives an instance of A, and maps it to an insert values statement
  */

final class Insert[A](query: Query)(using meta: ModelMeta[A])
    extends (A => Completable):
  def apply(entity: A): Completable =
    val foldedFields =
      SqlOperations.commaSeparatedParened(
        meta.map(entity)._1.toList
      )

    val foldedValues =
      SqlOperations.commaSeparatedParened(
        meta.map(entity)._2.toList
      )

    new Completable(
      query.copy(arguments =
        query.arguments ++ List(foldedFields, Arguments.values, foldedValues)
      )
    ) {}

object Insert:
  def apply[A: ModelMeta](query: Query) =
    new Insert(query)
