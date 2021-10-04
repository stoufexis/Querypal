package org.querypal.logic

import Model._

object Field:
  /** A field of A modeling a value member of type B
    */
  case class Column[A, B](private val name: String)(using
      toType: ToTypeDescription[B]
  ):
    val getName                   = name
    val toTypeDescription: String = toType.toTypeDescription(name) + " not null"

  case class PrimaryKey[A, B](field: Column[A, B])
