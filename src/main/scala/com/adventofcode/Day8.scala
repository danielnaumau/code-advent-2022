package com.adventofcode

import com.adventofcode.Common.{CharHelper, Matrix}

object Day8 {

  def main(args: Array[String]): Unit = {
    println(process(Task1.solve))
    println(process(Task2.solve))
  }

  def mapNeighbours[R](i: Int, j: Int, matrix: Matrix[Int])(
      fun: (List[List[Int]], Int) => R
  ): Option[R] =
    for {
      column <- matrix.getColumn(j)
      row <- matrix.getRow(i)
      value <- matrix.get(i, j)
      (rowLeft, rowRight) = row.splitAt(j)
      (columnLeft, columnRight) = column.splitAt(i)
      all = List(
        rowLeft.reverse,
        rowRight.drop(1),
        columnLeft.reverse,
        columnRight.drop(1)
      )
    } yield fun(all, value)

  object Task1 {
    def solve(matrix: Matrix[Int]): Int = {
      val res = for {
        i <- (1 until matrix.height - 1)
        j <- (1 until matrix.width - 1)
      } yield mapNeighbours(i, j, matrix)((neighbours, value) =>
        neighbours.exists(_.forall(_ < value))
      )

      val bestTrees = res.flatten.count(_ == true)
      bestTrees + (matrix.height * 2) + (matrix.width * 2 - 4)
    }
  }

  object Task2 {

    def getWeight(list: List[Int], value: Int): Int = {
      val index = list.indexWhere(_ >= value)
      if (index == -1) list.size else index + 1
    }

    def solve(matrix: Matrix[Int]): Int = {
      val res = for {
        i <- (1 until matrix.height - 1)
        j <- (1 until matrix.width - 1)
      } yield mapNeighbours(i, j, matrix)((neighbours, value) =>
        neighbours.map(getWeight(_, value)).product
      )

      res.flatten.toList.sorted.lastOption.getOrElse(0)
    }
  }

  def process(fun: Matrix[Int] => Int): Option[Int] =
    Common
      .readFile("src/main/resources/day8/task.txt", processMatrix)
      .map(fun)

  def processMatrix(lines: List[String]): Matrix[Int] = {
    val matrixWidth = lines.headOption.map(_.length).getOrElse(0)
    lines.foldLeft(Matrix.empty[Int](matrixWidth)) { case (matrix, line) =>
      matrix.addRow(line.toList.flatMap(_.toIntOption))
    }
  }
}
