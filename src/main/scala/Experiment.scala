// import FragmentOperations.Column
// import doobie.implicits._
// import scala.deriving.Mirror
// import scala.collection.View.Zip
// import FragmentOperations.Field
// // import Experiment.SetArgument
// import Experiment.Fragment
// import scala.util.Try

// object Experiment extends App {

//   case object Personn {
//     val name   = Column[String, Person](fr"name")
//     val age    = Column[Int, Person](fr"age")
//     val fields = (name, age)
//   }
//   case class family()
//   val asd = Person("asd", 123)

//   val asd2 = Tuple.fromProduct(asd)

//   case class Fragment[A](a: A) {
//     override val toString = a match
//       case x: Int    => s"'${x}': Integer"
//       case x: String => s"'${x}': String"
//   }

//   case class SetArg[A](fr: A, col: Column[_, _]) {
//     override val toString = s"$fr -> ${col.name}"
//   }

//   val mapper = [T] => (a: T) => Fragment[T](a)

//   val toSetArg = [T] =>
//     (fieldsFr: T) =>
//       fieldsFr match
//         case (first, sec): (T, Column[?, ?]) => SetArg[T](first, sec)

//   def as3d[A <: Product](tup: A)(using
//       m: Mirror.ProductOf[A]
//   ) = {
//     val aaa: Tuple = Tuple.fromProductTyped(tup)
//     aaa
//       .map(mapper)
//       .zip(Personn.fields)
//       .map(toSetArg)
//   }

//   // type MatchTuple <: Tuple = Tuple match
//   //   case EmptyTuple => EmptyTuple
//   //   case h *: t =>
//   //     Experiment.Fragment[h] *: scala.Tuple.Map[t, Experiment.Fragment]

//   // type ZippedWithFields =
//   //   Tuple.Zip[MatchTuple, (Column[String, Person], Column[Int, Person])]

//   as3d[Person](asd).toList.foreach(println(_))
// }
