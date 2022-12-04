package com.adventofcode

import com.adventofcode.Common.NonEmptyList


object Day1 {
  def task1(calories: List[Int]): Int =
    calories.max

  def task2(calories: List[Int]): Int =
    calories.sorted.reverse.take(3).sum

  def convert(lines: List[String]): List[Int] = {
    lines.foldRight(NonEmptyList(0)) { case (line, values) =>
      if (line.isEmpty)
        0 +: values
      else {
        NonEmptyList(values.head + line.toIntOption.getOrElse(0), values.tail)
      }
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val calories = Common.readFile("src/main/resources/day1/task.txt", convert)

    println(calories.map(task1))
    println(calories.map(task2))
  }
}
