package com.adventofcode

import com.adventofcode.Common.ListTraverse

object Day11 {
  final case class MonkeyId(value: String) extends AnyVal
  final case class Item(value: Long) extends AnyVal
  sealed trait Operation {
    def changeValue(item: Item, modOpt: Option[Int]): Item
  }

  final case class AddTo(value: Option[Long]) extends Operation {
    override def changeValue(item: Item, modOpt: Option[Int]): Item =
      modOpt
        .map(mod => Item((item.value + value.getOrElse(item.value)) % mod))
        .getOrElse(Item((item.value + value.getOrElse(item.value)) / 3))
  }

  final case class MultiplyBy(value: Option[Long]) extends Operation {
    override def changeValue(item: Item, modOpt: Option[Int]): Item =
      modOpt
        .map(mod => Item((item.value * value.getOrElse(item.value)) % mod))
        .getOrElse(Item((item.value * value.getOrElse(item.value)) / 3))
  }

  final case class MonkeyAction(
      id: MonkeyId,
      items: List[Item],
      operation: Operation,
      divisibleBy: Int,
      ifTrue: MonkeyId,
      ifFalse: MonkeyId,
      allItemsAmount: Long
  ) {
    def inspectedItemsAmount: Long =
      allItemsAmount - items.size

    def receiveItems(newItems: List[Item]): MonkeyAction =
      this.copy(
        items = items ++ newItems,
        allItemsAmount = allItemsAmount + newItems.length
      )

    def cleanItems: MonkeyAction =
      this.copy(items = List.empty)
  }

  final case class State(actions: Map[MonkeyId, MonkeyAction]) {
    def processAction(monkeyId: MonkeyId, mod: Option[Int]): State = {
      actions
        .get(monkeyId)
        .map { action =>
          val newItems = action.items.map(action.operation.changeValue(_, mod))
          val dividedItems = newItems.filter(_.value % action.divisibleBy == 0)
          val notDividedItems =
            newItems.filter(_.value % action.divisibleBy != 0)

          val res = actions
            .updatedWith(monkeyId)(_.map(_.cleanItems))
            .updatedWith(action.ifTrue)(_.map(_.receiveItems(dividedItems)))
            .updatedWith(action.ifFalse)(_.map(_.receiveItems(notDividedItems)))

          State(res)
        }
        .getOrElse(this)
    }
  }

  def parseActions(lines: List[String]): Option[MonkeyAction] = {
    val parseMonkeyId: String => Option[MonkeyId] = {
      case s"Monkey $id:" => Some(MonkeyId(id))
      case _              => None
    }

    val parseItems: String => Option[List[Item]] = {
      case s"Starting items: $items" =>
        items
          .split(", ")
          .toList
          .traverse(item => item.toLongOption.map(Item))

      case _ => None
    }

    val parseOperation: String => Option[Operation] = {
      case s"Operation: new = old + $value" =>
        Some(AddTo(value.toLongOption))
      case s"Operation: new = old * $value" =>
        Some(MultiplyBy(value.toLongOption))
      case _ => None
    }

    val parseDivisibleBy: String => Option[Int] = {
      case s"Test: divisible by $value" => value.toIntOption
      case _                            => None
    }

    def parseIfTrue: String => Option[MonkeyId] = {
      case s"If true: throw to monkey $value" => Some(MonkeyId(value))
      case _                                  => None
    }

    def parseIfFalse: String => Option[MonkeyId] = {
      case s"If false: throw to monkey $value" => Some(MonkeyId(value))
      case _                                   => None
    }

    for {
      monkeyId <- lines.headOption.flatMap(parseMonkeyId)
      items <- lines.lift(1).flatMap(parseItems)
      operation <- lines.lift(2).flatMap(parseOperation)
      divisibleBy <- lines.lift(3).flatMap(parseDivisibleBy)
      ifTrue <- lines.lift(4).flatMap(parseIfTrue)
      ifFalse <- lines.lift(5).flatMap(parseIfFalse)
    } yield MonkeyAction(
      monkeyId,
      items,
      operation,
      divisibleBy,
      ifTrue,
      ifFalse,
      items.length
    )
  }

  def runCycle(
      actions: List[MonkeyAction],
      mod: Option[Int]
  ): List[MonkeyAction] =
    actions
      .map(_.id)
      .sortBy(_.value)
      .foldLeft(State(actions.map(action => action.id -> action).toMap))(
        _.processAction(_, mod)
      )
      .actions
      .values
      .toList

  def runCycles(
      actions: List[MonkeyAction],
      cycles: Int,
      useMod: Boolean
  ): Long = {
    val mod = Option.when(useMod)(actions.map(_.divisibleBy).product)
    (0 until cycles).toList
      .foldLeft(actions)((curActions, _) => runCycle(curActions, mod))
      .map(_.inspectedItemsAmount)
      .sorted(Ordering[Long].reverse)
      .slice(0, 2)
      .product
  }

  def parse(lines: List[String]): List[MonkeyAction] =
    lines
      .map(_.trim)
      .grouped(7)
      .flatMap(parseActions)
      .toList

  def solve(cycles: Int, useMod: Boolean): Option[Long] =
    Common
      .readFile("src/main/resources/day11/task.txt", parse)
      .map(runCycles(_, cycles, useMod))

  def main(args: Array[String]): Unit = {
    println(solve(20, useMod = false))
    println(solve(10000, useMod = true))
  }
}
