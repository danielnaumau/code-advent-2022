package com.adventofcode

import com.adventofcode.Common.{Coordinate, Matrix, MatrixValue}

import scala.annotation.tailrec

object Day12 {
  def readMatrix(lines: List[String]): Matrix[Char] =
    lines
      .foldLeft(
        Matrix.empty[Char](lines.headOption.map(_.length).getOrElse(0))
      )((matrix, line) => matrix.addRow(line.toList))

  final case class StateBFS(
      values: List[MatrixValue[Char]],
      visited: Set[Coordinate],
      cyclesAmount: Int
  )

  def checkNeighbours(
      neighbour: MatrixValue[Char],
      cur: MatrixValue[Char],
      visited: Set[Coordinate]
  ): Boolean = {
    val neighbourValue =
      if (neighbour.value == 'E')
        'z'.toInt
      else
        neighbour.value.toInt

    cur.value.toInt - neighbourValue >= -1 &&
    !visited.contains(neighbour.coordinate)
  }

  def bfs(matrix: Matrix[Char], initial: MatrixValue[Char]): Int = {
    @tailrec
    def bfs0(state: StateBFS): Int = state.values match {
      case Nil                                     => -1
      case values if values.exists(_.value == 'E') => state.cyclesAmount
      case values =>
        val neighbours = values.flatMap { cur =>
          matrix
            .neighbours(cur.coordinate.y, cur.coordinate.x)
            .filter(checkNeighbours(_, cur, state.visited))
        }.toSet

        bfs0(
          StateBFS(
            neighbours.toList,
            state.visited ++ neighbours.map(_.coordinate),
            state.cyclesAmount + 1
          )
        )
    }

    bfs0(
      StateBFS(List(initial.copy(value = 'a')), Set(initial.coordinate), 0)
    )
  }

  object Task1 {
    def solve(matrix: Matrix[Char]): Option[Int] =
      matrix.find('S').map(bfs(matrix, _))
  }

  object Task2 {
    def solve(matrix: Matrix[Char]): Option[Int] =
      matrix.findAll('a').map(bfs(matrix, _)).filter(_ != -1).minOption
  }

  def process(solve: Matrix[Char] => Option[Int]): Option[Int] =
    Common
      .readFile("src/main/resources/day12/task.txt", readMatrix)
      .flatMap(solve)

  def main(args: Array[String]): Unit = {
    println(process(Task1.solve))
    println(process(Task2.solve))
  }
}
