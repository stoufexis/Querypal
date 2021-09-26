import doobie.util.fragment.Fragment
import doobie.implicits._
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import Common._
import FragmentOperations._

/** Automatic derivation of instances of the ModelMeta type class for entity A
  */
object DeriveModelMeta:

  trait ToFragment[A] {
    def toFragment(a: A): Fragment
  }

  given intToFragment: ToFragment[Int] with {
    def toFragment(a: Int): Fragment = sql"${a}"
  }

  given stringToFragment: ToFragment[String] with {
    def toFragment(a: String): Fragment = sql"${a}"
  }

  inline def getTypeclassInstances[A <: Tuple]: List[ToFragment[Any]] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        val headTypeClass = summonInline[ToFragment[head]]

        val tailTypeClasses = getTypeclassInstances[tail]

        headTypeClass
          .asInstanceOf[ToFragment[Any]] :: getTypeclassInstances[tail]
    }

  inline def summonInstancesHelper[A](using m: Mirror.Of[A]) =
    getTypeclassInstances[m.MirroredElemTypes]

  inline def deriveModelMeta[A](using
      m: Mirror.ProductOf[A]
  )(tableName: Fragment)(pk: Fragment)(fields: Fragment*) =
    new ModelMeta[A] {
      val table          = Table(tableName)
      val primaryKeyName = pk

      def map(a: A): (Iterator[Fragment], Iterator[Fragment]) = {
        val elemInstances = getTypeclassInstances[m.MirroredElemTypes]
        val elems         = a.asInstanceOf[Product].productIterator

        val elemStrings = elems.zip(pk +: fields).zip(elemInstances).map {
          case ((elem, field), instance) =>
            (field, instance.toFragment(elem))
        }
        (elemStrings.map(_._1), elemStrings.map(_._2))
      }
    }
