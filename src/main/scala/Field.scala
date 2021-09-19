import doobie.implicits._
import doobie.util.fragment.Fragment

case class Field[+B](name: Fragment)

trait FieldOps[A]:

  extension (x: Field[Int]) def gt(y: Int) = sql"${x.name} > $y "

  extension (x: Field[Int]) def lt(y: Int) = sql"${x.name} < $y "

  extension (x: Field[A])
    def eqls(y: A) = y match
      case z: Int    => sql"${x.name} = ${(z: Int)} "
      case z: String => sql"${x.name} = ${(z: String)} "

// extension (x: Field[A])
//   def value(y: A): FieldValue = y match
//     case z: Int    => FieldValue(x, sql"${(z: Int)}")
//     case z: String => FieldValue(x, sql"${(z: String)}")

// extension (x: Field[A])
//   def set(y: A): Setting = y match
//     case z: Int    => sql"${x.name} = ${(z: Int)} "
//     case z: String => sql"${x.name} = ${(z: String)} "

// extension (x: FieldValue | List[FieldValue])
//   def and(y: FieldValue): List[FieldValue] = x match
//     case z: FieldValue       => List(z, y)
//     case z: List[FieldValue] => z :+ y

// extension (x: Setting | List[Setting])
//   def and(y: Setting): List[Setting] = x match
//     case z: Setting       => List(z, y)
//     case z: List[Setting] => z :+ y

end FieldOps

given [A]: FieldOps[A] with {}
