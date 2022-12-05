package com.adventofcode

object Day3 {

  val lowercase: Set[Char] = ('a' to 'z').toSet
  val uppercase: Set[Char] = ('A' to 'Z').toSet

  def main(args: Array[String]): Unit = {
    println(solve(Task1.commonItems))
    println(solve(Task2.commonItems))
  }

  def solve(commonItems: List[String] => List[Char]): Option[Int] =
    Common
      .readFile("src/main/resources/day3/task.txt", commonItems)
      .map(_.map(calculatePriority).sum)


  def calculatePriority(char: Char): Int =
    if (lowercase.contains(char))
      char.toInt - 96
    else if (uppercase.contains(char))
      char.toInt - 38
    else
      0


  object Task2 {
    def commonItems(lines: List[String]): List[Char] =
      lines.grouped(3).flatMap(line => intersect(line.map(_.toSet))).toList
  }

  object Task1 {
    private def split(line: String): List[Set[Char]] = {
      val (left, right) = line.splitAt(line.length / 2)
      List(left.toSet, right.toSet)
    }

    def commonItems(lines: List[String]): List[Char] =
      lines.flatMap(line => intersect(split(line)))
  }

  def intersect(values: List[Set[Char]]): Set[Char] =
    values.fold(values.headOption.getOrElse(Set.empty))((res, cur) => res.intersect(cur))

}