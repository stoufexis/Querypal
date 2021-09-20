import doobie.util.fragment.Fragment
import doobie.implicits._
import Common._
import cats.implicits._
import FragmentOperations._

final class Update[A, B <: Fields](
    query: Query,
    model: Model[A, B]
):
  def apply(
      f: (A => List[FieldValue]) => List[FieldValue]
  ): Where[A, B] =
    Where(
      query.copy(arguments =
        query.arguments :+ SqlOperations.setArgument(f(model.meta.mapper))
      ),
      model
    )

object Update:
  def apply[A, B <: Fields](
      query: Query,
      model: Model[A, B]
  ) = new Update(query, model)
