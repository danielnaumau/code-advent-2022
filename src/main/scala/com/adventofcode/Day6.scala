package com.adventofcode

object Day6 {
  def main(args: Array[String]): Unit = {
    println(solve(4))
    println(solve(14))
  }

  def solve(size: Int): Option[Int] =
    Common
      .readFile("src/main/resources/day6/task.txt", convert)
      .map(findIndex(_, size))

  def findIndex(signal: String, size: Int): Int =
    signal.sliding(size, 1).indexWhere(_.toSet.size == size) + size

  def convert(lines: List[String]): String =
    lines.mkString
}
