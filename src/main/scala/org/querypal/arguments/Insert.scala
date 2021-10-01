package arguments

import logic.FragmentOperations._

import logic.Model._
import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import logic.FragmentOperations.Completable
import logic.Query

/** Insert receives an instance of A, and maps it to an insert values statement
  */
final class Insert[A](query: Query)(using meta: ModelMeta[A])
    extends (A => Completable) { self =>
  def apply(entity: A): Completable =
    val foldedFields =
      SqlOperations.commaSeparatedParened(
        meta.map(entity)._1.toList
      )
    val foldedValues =
      SqlOperations.commaSeparatedParened(
        meta.map(entity)._2.toList
      )

    new Completable {
      val query = self.query.copy(arguments =
        self.query.arguments :+ foldedFields :+ Arguments.values :+ foldedValues
      )

      def complete: Argument = query.complete

      def construct: Fragment = query.construct

      def constructString: String = query.constructString
    }
}
