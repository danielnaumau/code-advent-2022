package com.adventofcode

import com.adventofcode.Common.Matrix

object Day5 {
  final case class Crates(values: List[List[Char]]) {
    def move(command: Command): Option[Crates] =
      for {
        fromValues <- values.lift(command.from)
        toValues <- values.lift(command.to)
        fromNew = fromValues.dropRight(command.amount)
        toNew = toValues ++ fromValues.takeRight(
          command.amount
        ) // .reverse - for the first task
        newValues = values
          .updated(command.from, fromNew)
          .updated(command.to, toNew)
      } yield Crates(newValues)
  }
  final case class Command(from: Int, to: Int, amount: Int)

  def main(args: Array[String]): Unit = {
    println(solve())
  }

  def solve(): Option[String] =
    for {
      crates <- Common.readFile(
        "src/main/resources/day5/crates.txt",
        readCrates
      )
      commands <- Common.readFile(
        "src/main/resources/day5/moves.txt",
        readCommands
      )
      res = executeCommands(crates, commands)
      topCrates = res.values.map(_.lastOption.getOrElse(' ')).mkString
    } yield topCrates

  def executeCommands(crates: Crates, commands: List[Command]): Crates =
    commands.foldLeft(crates) { case (state, command) =>
      state.move(command).getOrElse(state)
    }

  def readCommands(lines: List[String]): List[Command] = {
    lines.collect { case s"move $amountStr from $fromStr to $toStr" =>
      for {
        from <- fromStr.toIntOption
        amount <- amountStr.toIntOption
        to <- toStr.toIntOption
      } yield Command(from - 1, to - 1, amount)
    }.flatten
  }

  def readCrates(lines: List[String]): Crates = {
    val reversed = lines.reverse
    val length = reversed.headOption.map(_.trim.split(" +").length).getOrElse(0)
    val initial = Matrix.empty[Option[Char]](length)
    val symbols = reversed.drop(1)
    val matrix = symbols.foldLeft(initial) { case (state, line) =>
      val symbolsRow = s"$line ".grouped(4).map(processSymbol).toList
      state.addRow(
        symbolsRow ++ List.fill[Option[Char]](length - symbolsRow.length)(None)
      )
    }

    Crates(matrix.values.map(_.flatten))
  }

  def processSymbol(symbolStr: String): Option[Char] =
    symbolStr.toList match {
      case '[' :: symbol :: ']' :: ' ' :: Nil => Some(symbol)
      case _                                  => None
    }
}
