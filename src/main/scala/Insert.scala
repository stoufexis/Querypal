import FragmentOperations._
import Common._

import doobie.implicits._
import cats.Monoid
import cats.implicits._
import doobie.util.fragment.Fragment
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait ToFragment[A] {
  def toFragment(a: A): Fragment
}

given intToFragment: ToFragment[Int] with {
  def toFragment(a: Int): Fragment = sql"${a}"
}

given stringToFragment: ToFragment[String] with {
  def toFragment(a: String): Fragment = sql"${a}"
}

trait Mapper[A] {
  def map(a: A): (Iterator[Fragment], Iterator[Fragment])
}

inline def getTypeclassInstances[A <: Tuple]: List[ToFragment[Any]] =
  inline erasedValue[A] match {
    case _: EmptyTuple     => Nil
    case _: (head *: tail) =>
      // summon was known as implicitly in scala 2
      val headTypeClass = summonInline[ToFragment[head]]

      // recursive call to resolve also the tail
      val tailTypeClasses = getTypeclassInstances[tail]

      headTypeClass
        .asInstanceOf[ToFragment[Any]] :: getTypeclassInstances[tail]
  }

inline def summonInstancesHelper[A](using m: Mirror.Of[A]) =
  getTypeclassInstances[m.MirroredElemTypes]

inline def deriveMapper[A](using
    m: Mirror.ProductOf[A],
    meta: ModelMeta[A]
) =
  new Mapper[A] {
    def map(a: A): (Iterator[Fragment], Iterator[Fragment]) = {
      val elemInstances = getTypeclassInstances[m.MirroredElemTypes]
      val elems         = a.asInstanceOf[Product].productIterator

      val elemStrings = elems.zip(meta.fields).zip(elemInstances).map {
        case ((elem, field), instance) =>
          (field, instance.toFragment(elem))
      }
      (elemStrings.map(_._1), elemStrings.map(_._2))
    }
  }

final class Insert[A <: Product, B <: Model[A]](model: B)(query: Query)(using
    meta: ModelMeta[A],
    mapper: Mapper[A]
):

  def apply(entity: A)(using
      m: Mirror.ProductOf[A]
  ): Completable =

    val foldedFields =
      SqlOperations.commaSeparatedParened(
        mapper.map(entity)._1.toList
      )

    val foldedValues =
      SqlOperations.commaSeparatedParened(
        mapper.map(entity)._2.toList
      )

    new Completable(
      query.copy(arguments =
        query.arguments ++ List(foldedFields, Arguments.values, foldedValues)
      )
    ) {}

object Insert:
  def apply[A <: Product: ModelMeta: Mapper, B <: Model[A]](model: B)(
      query: Query
  ) =
    new Insert(model)(query)
