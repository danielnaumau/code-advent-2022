package com.adventofcode

import com.adventofcode.Common.{Coordinate, Matrix, MatrixValue}

object Day14 {
  val FIRST_SAND: Coordinate = Coordinate(500, 0)

  def readCoordinate(line: String): Option[Coordinate] =
    line.split(",").toList match {
      case xStr :: yStr :: Nil =>
        for {
          x <- xStr.toIntOption
          y <- yStr.toIntOption
        } yield Coordinate(x, y)
      case _ =>
        None
    }

  def range(left: Int, right: Int): List[Int] =
    (Math.min(left, right) to Math.max(left, right)).toList

  def fillStones(
      start: Coordinate,
      end: Coordinate
  ): List[MatrixValue[Char]] =
    for {
      y <- range(start.y, end.y)
      x <- range(start.x, end.x)
    } yield MatrixValue('#', Coordinate(x, y))

  def readPath(line: String): List[MatrixValue[Char]] =
    line
      .split(" -> ")
      .toList
      .sliding(2, 1)
      .toList
      .flatMap {
        case startStr :: endStr :: Nil =>
          for {
            start <- readCoordinate(startStr)
            end <- readCoordinate(endStr)
          } yield fillStones(start, end)
        case _ => None
      }
      .flatten

  def readCave(lines: List[String], withCoveredFloor: Boolean): Matrix[Char] = {
    val height = 179
    val width = 1000
    val initial = Matrix.fill[Char](height, width)(' ')
    val res =
      lines.foldLeft(initial)((matrix, line) => matrix.update(readPath(line)))
    if (withCoveredFloor)
      res.update(
        fillStones(Coordinate(0, height - 1), Coordinate(width, height - 1))
      )
    else
      res
  }

  final case class State(
      matrix: Matrix[Char],
      curSand: Coordinate = FIRST_SAND,
      sandAmount: Int = 0
  ) {
    def fallDown: State = {
      val newSand = curSand.copy(y = curSand.y + 1)

      this.copy(
        matrix = updateMatrix(curSand, newSand),
        curSand = newSand
      )
    }

    def updateMatrix(
        previousSand: Coordinate,
        newSand: Coordinate
    ): Matrix[Char] =
      matrix
        .update(MatrixValue(' ', previousSand))
        .update(MatrixValue('o', newSand))

    def fallDiagonal(toLeft: Boolean): State = {
      val newSand =
        if (toLeft)
          curSand.copy(x = curSand.x - 1, y = curSand.y + 1)
        else
          curSand.copy(x = curSand.x + 1, y = curSand.y + 1)

      this.copy(
        matrix = updateMatrix(curSand, newSand),
        curSand = newSand
      )
    }

    def canFallDown: Boolean =
      matrix.get(curSand.y + 1, curSand.x).contains(' ')

    def canFallLeft: Boolean =
      matrix.get(curSand.y + 1, curSand.x - 1).contains(' ')

    def canFallRight: Boolean =
      matrix.get(curSand.y + 1, curSand.x + 1).contains(' ')

    def finished: Boolean =
      List(
        matrix.get(curSand.y + 1, curSand.x),
        matrix.get(curSand.y + 1, curSand.x - 1),
        matrix.get(curSand.y + 1, curSand.x + 1)
      ).exists(_.isEmpty)

    def sourceBlocked: Boolean =
      List(
        matrix.get(FIRST_SAND.y, FIRST_SAND.x),
        matrix.get(FIRST_SAND.y + 1, FIRST_SAND.x),
        matrix.get(FIRST_SAND.y + 1, FIRST_SAND.x - 1),
        matrix.get(FIRST_SAND.y + 1, FIRST_SAND.x + 1)
      ).forall(_.contains('o'))

    def newSand: State =
      this.copy(
        matrix = matrix.update(MatrixValue('o', FIRST_SAND)),
        curSand = FIRST_SAND,
        sandAmount = sandAmount + 1
      )
  }

  def fallSand(matrix: Matrix[Char], withCoveredFloor: Boolean): Int = {
    def fallSand0(state: State): Int = {
      if (state.canFallDown)
        fallSand0(state.fallDown)
      else if (state.canFallLeft)
        fallSand0(state.fallDiagonal(true))
      else if (state.canFallRight)
        fallSand0(state.fallDiagonal(false))
      else if (state.finished)
        state.sandAmount
      else if (withCoveredFloor && state.sourceBlocked)
        state.sandAmount
      else
        fallSand0(state.newSand)
    }

    fallSand0(State(matrix))
  }

  def process(withCoveredFloor: Boolean) =
    Common
      .readFile(
        "src/main/resources/day14/task.txt",
        readCave(_, withCoveredFloor)
      )
      .map(fallSand(_, withCoveredFloor))

  def main(args: Array[String]): Unit = {
    println(process(false))
    println(process(true).map(_ + 1))
  }
}
