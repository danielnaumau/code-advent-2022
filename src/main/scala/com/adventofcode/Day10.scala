package com.adventofcode

import com.adventofcode.Common.NonEmptyList

object Day10 {
  def main(args: Array[String]): Unit = {
    val signals = Common.readFile("src/main/resources/day10/task.txt", process)
    println(signals.map(Task1.solve))
    signals.map(Task2.solve).foreach(Task2.print)
  }

  object Task1 {
    def solve(signals: List[Int]): Int = {
      val cycles = List(20, 60, 100, 140, 180, 220)
      cycles.map(cycle => signals.lift(cycle - 1).getOrElse(0) * cycle).sum
    }
  }

  object Task2 {
    def solve(signals: List[Int]): List[List[Boolean]] =
      List((1, 40), (41, 80), (81, 120), (121, 160), (161, 200), (201, 240))
        .map { case (start, end) => runCycle(signals, start, end) }

    def runCycle(signals: List[Int], start: Int, end: Int): List[Boolean] =
      (start to end).toList.map { cycle =>
        signals
          .lift(cycle - 1)
          .exists(signal =>
            cycle - start + 1 >= signal && cycle - start + 1 <= signal + 2
          )
      }

    def print(signals: List[List[Boolean]]): Unit =
      println(
        signals
          .map(_.map(value => if (value) "#" else " ").mkString(""))
          .mkString("\n")
      )
  }

  final case class Command(cycle: Int, value: Int)

  def processSignal(
      signals: NonEmptyList[Int],
      line: String
  ): NonEmptyList[Int] = line match {
    case "noop" => signals :+ signals.last
    case s"addx $value" =>
      signals ++ List(
        signals.last,
        signals.last + value.toIntOption.getOrElse(0)
      )
    case _ => signals
  }

  def process(lines: List[String]): List[Int] =
    lines.foldLeft(NonEmptyList(1))(processSignal).toList
}
