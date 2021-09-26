import doobie.implicits._
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import Common._
import FragmentOperations._

/** Automatic derivation of instances of the ModelMeta type class for entity A
  */
object DeriveModelMeta:

  trait ToString[A] {
    def ToString(a: A): String
  }

  given intToString: ToString[Int] with {
    def ToString(a: Int): String = s"${a}"
  }

  given stringToString: ToString[String] with {
    def ToString(a: String): String = s"'${a}'"
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

  inline def getTypeclassInstances[A <: Tuple]: List[ToString[Any]] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        val headTypeClass = summonInline[ToString[head]]

        val tailTypeClasses = getTypeclassInstances[tail]

        headTypeClass
          .asInstanceOf[ToString[Any]] :: getTypeclassInstances[tail]
    }

  inline def summonInstancesHelper[A](using m: Mirror.Of[A]) =
    getTypeclassInstances[m.MirroredElemTypes]

  inline def deriveModelMeta[A](using
      m: Mirror.ProductOf[A]
  )(tableName: String) =
    new ModelMeta[A] {
      val table          = Table(tableName)
      val primaryKeyName = getElemLabels[m.MirroredElemLabels](0)

      def map(a: A): (Iterator[String], Iterator[String]) = {
        val elemInstances = getTypeclassInstances[m.MirroredElemTypes]
        val elemLabels    = getElemLabels[m.MirroredElemLabels]
        val elems         = a.asInstanceOf[Product].productIterator

        val elemStrings = elems.zip(elemLabels).zip(elemInstances).map {
          case ((elem, label), instance) =>
            (label, instance.ToString(elem))
        }
        (elemStrings.map(_._1), elemStrings.map(_._2))
      }
    }
