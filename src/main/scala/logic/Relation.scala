package logic

import Field._
import Model._
import FragmentOperations._
import FragmentOperations.Argument._
import cats.kernel.Monoid
import cats.implicits._

object Relation {

  /** A type level declaration of a Relation between A and B, used to validate
    * the existence of a relation during compile time
    */
  trait Relation[A, B](val fk: Column[A, ?])(using
      fromMeta: ModelMeta[A],
      toMeta: ModelMeta[B]
  ):
    val joinCondition: Argument =
      fromMeta.table.name
        |+| GeneralOperators.dot
        |+| fk.name.toArgument
        |+| GeneralOperators.equals
        |+| toMeta.table.name
        |+| GeneralOperators.dot
        |+| toMeta.primaryKeyName.toArgument

}
