package com.adventofcode

import com.adventofcode.Common.{Coordinate, NonEmptyList}

object Day9 {

  def main(args: Array[String]): Unit = {
    println(solve(1))
    println(solve(9))
  }

  def solve(tailSize: Int): Option[Int] =
    Common
      .readFile("src/main/resources/day9/task.txt", process(_, tailSize))
      .map(_.visited.size)

  final case class State(
      visited: Set[Coordinate],
      rope: NonEmptyList[Coordinate]
  )

  object State {
    def initial(tailSize: Int): State =
      State(
        Set(Coordinate(0, 0)),
        NonEmptyList.fill(tailSize)(Coordinate(0, 0))
      )
  }

  def process(lines: List[String], tailSize: Int): State =
    lines.foldLeft(State.initial(tailSize)) { (state, line) =>
      decodeCommand(line)
        .map(command => move(state, command.direction, command.movesAmount))
        .getOrElse(state)
    }

  def decodeCommand(line: String): Option[MoveCommand] = line match {
    case s"$directionStr $movesAmountStr" =>
      for {
        direction <- Direction.read(directionStr)
        movesAmount <- movesAmountStr.toIntOption
      } yield MoveCommand(direction, movesAmount)
    case _ =>
      None
  }

  final case class MoveCommand(direction: Direction, movesAmount: Int)

  def moveTail(head: Coordinate, tail: Coordinate): Coordinate = {
    val dx = head.x - tail.x
    val dy = head.y - tail.y
    if (Math.abs(dx) <= 1 && Math.abs(dy) <= 1)
      tail
    else if (dx == 0)
      tail.copy(y = tail.y + (dy / Math.abs(dy)))
    else if (dy == 0)
      tail.copy(x = tail.x + (dx / Math.abs(dx)))
    else
      Coordinate(
        y = tail.y + (dy / Math.abs(dy)),
        x = tail.x + (dx / Math.abs(dx))
      )
  }

  def move(state: State, direction: Direction, movesAmount: Int): State =
    (0 until movesAmount).foldLeft(state)((state, _) => move(state, direction))

  def move(state: State, direction: Direction): State = {
    import state._
    val newHead = direction.moveHead(rope.head)

    val newRope =
      rope.tail.foldLeft(NonEmptyList(newHead))((allMoves, curMove) =>
        allMoves :+ moveTail(allMoves.last, curMove)
      )

    State(
      visited + newRope.last,
      newRope
    )
  }

  sealed trait Direction {
    def moveHead(coordinate: Coordinate): Coordinate
  }

  object Direction {
    case object Left extends Direction {
      override def moveHead(coordinate: Coordinate): Coordinate =
        coordinate.copy(x = coordinate.x - 1)
    }

    case object Right extends Direction {
      override def moveHead(coordinate: Coordinate): Coordinate =
        coordinate.copy(x = coordinate.x + 1)
    }

    case object Up extends Direction {
      override def moveHead(coordinate: Coordinate): Coordinate =
        coordinate.copy(y = coordinate.y + 1)
    }

    case object Down extends Direction {
      override def moveHead(coordinate: Coordinate): Coordinate =
        coordinate.copy(y = coordinate.y - 1)
    }

    def read(value: String): Option[Direction] = value match {
      case "U" => Some(Direction.Up)
      case "R" => Some(Direction.Right)
      case "L" => Some(Direction.Left)
      case "D" => Some(Direction.Down)
      case _   => None
    }
  }
}
