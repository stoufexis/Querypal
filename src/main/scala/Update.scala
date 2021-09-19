import doobie.util.fragment.Fragment
// import Common._

type Setting = Fragment

// final class Update[A <: Model](command: String, table: String, model: A) {
//   def apply(f: A => List[Setting]): NonCompletableWhere[A] =
//     Where(
//       command,
//       table,
//       "set " +: List(f(model).mkString(", ").dropRight(1)),
//       model
//     )

// }

// object Update {
//   def apply[A <: Model](command: String, table: String, model: A) =
//     new Update(command, table, model)

// }
