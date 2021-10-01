package org.querypal.logic

import FragmentOperations._
import FragmentOperations.Argument._
import FragmentOperations.ConditionOperator
import cats.implicits._
import cats.kernel.Monoid

case class ConditionList(
    conditions: List[List[Argument]] = List(List[Argument]())
)

object ConditionList:
  def apply(arg: Argument): ConditionList = ConditionList(List(List(arg)))

  extension (conditionList: ConditionList)
    def addToLast(argument: Argument*): ConditionList = ConditionList(
      conditionList.conditions.dropRight(1)
        :+ (conditionList.conditions.last ++ argument)
    )

    def addList: ConditionList = ConditionList(
      conditionList.conditions :+ List[Argument]()
    )

    def last: List[Argument] = conditionList.conditions.last

    def dropRightFromLast(n: Int) = ConditionList(
      conditionList.conditions.dropRight(1)
        :+ (conditionList.conditions.last.dropRight(n))
    )

    def fold: Argument =
      val conditions = conditionList.conditions
        .flatMap(cond =>
          if (cond.length < 1) List(Monoid[Argument].empty)
          else List(cond.foldArgs, ConditionOperators.and)
        )
        .filter(_ != Monoid[Argument].empty)

      if (conditions.length > 0)
        FragmentOperations.Arguments.where
          |+| conditions.dropRight(1).foldArgs
      else
        Monoid[Argument].empty
