package logic

import doobie.implicits._
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import FragmentOperations._
import Field._
import Model._

/** Automatic derivation of instances of the ModelMeta type class for entity A
  */
object DeriveModelMeta:

  trait ToDoobieString[A] {
    def toDoobieString(a: A): String
  }

  given ToDoobieString[Int] with {
    def toDoobieString(a: Int): String = s"${a}"
  }

  given ToDoobieString[String] with {
    def toDoobieString(a: String): String = s"'${a}'"
  }

  inline def getElemLabels[A <: Tuple]: List[String] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        val headElementLabel  = constValue[head].toString
        val tailElementLabels = getElemLabels[tail]
        headElementLabel :: tailElementLabels
    }

  inline def getElemLabelsHelper[A](using m: Mirror.Of[A]) =
    getElemLabels[m.MirroredElemLabels]

  inline def getTypeclassInstances[A <: Tuple]: List[ToDoobieString[Any]] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        val headTypeClass = summonInline[ToDoobieString[head]]

        val tailTypeClasses = getTypeclassInstances[tail]

        headTypeClass
          .asInstanceOf[ToDoobieString[Any]] :: getTypeclassInstances[tail]
    }

  inline def summonInstancesHelper[A](using m: Mirror.Of[A]) =
    getTypeclassInstances[m.MirroredElemTypes]

  inline def deriveModelMeta[A](using
      m: Mirror.ProductOf[A]
  )(tableName: String)(fields: Seq[Column[?, ?]]) =
    new ModelMeta[A] {
      val table            = Table(tableName)
      val primaryKeyName   = getElemLabels[m.MirroredElemLabels](0)
      val typeDescriptions = fields.map(_.toTypeDescription)

      def map(a: A): (Seq[String], Seq[String]) = {
        val elemInstances = getTypeclassInstances[m.MirroredElemTypes]
        val elems         = a.asInstanceOf[Product].productIterator.toSeq

        val elemStrings = elems.zip(fields).zip(elemInstances).map {
          case ((elem, label), instance) =>
            (label, instance.toDoobieString(elem))
        }
        (elemStrings.map(_._1.toString), elemStrings.map(_._2))
      }
    }
