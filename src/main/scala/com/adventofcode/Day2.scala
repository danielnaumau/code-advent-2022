package com.adventofcode

object Day2 {

  def main(args: Array[String]): Unit = {
    println(solve(Task1.score))
    println(solve(Task2.score))
  }

  def solve(score: (Char, Char) => Option[Int]): Option[Int] =
    Common
      .readFile("src/main/resources/day2/task.txt", convert(score))
      .map(_.sum)

  def convert(score: (Char, Char) => Option[Int])(lines: List[String]): List[Int] =
    lines.flatMap {
      _.toCharArray.toList match {
        case left :: ' ' :: right :: Nil =>
          score(left, right)
        case _                           => None
      }
    }

  def calculateScore(leftMove: Move, rightMove: Move): Int = {
    if (rightMove.beats == leftMove)
      rightMove.score + 6
    else if (rightMove == leftMove)
      rightMove.score + 3
    else
      rightMove.score
  }

  object Task1 {
    private def read(value: Char): Option[Move] =
      value match {
        case 'A' | 'X' => Some(Move.Rock)
        case 'B' | 'Y' => Some(Move.Paper)
        case 'C' | 'Z' => Some(Move.Scissors)
        case _         => None
      }

    def score(leftChar: Char, rightChar: Char): Option[Int] =
      for {
        leftMove <- read(leftChar)
        rightMove <- read(rightChar)
      } yield calculateScore(leftMove, rightMove)
    }


  object Task2 {

    private def readFirstMove(value: Char): Option[Move] =
      value match {
        case 'A' => Some(Move.Rock)
        case 'B' => Some(Move.Paper)
        case 'C' => Some(Move.Scissors)
        case _   => None
      }

    private def readSecondMove(value: Char, firstMove: Move): Option[Move] =
      value match {
        case 'X' => Some(firstMove.beats)
        case 'Y' => Some(firstMove)
        case 'Z' => Some(firstMove.losesTo)
        case _   => None
      }

    def score(leftChar: Char, rightChar: Char): Option[Int] =
      for {
        leftMove <- readFirstMove(leftChar)
        rightMove <- readSecondMove(rightChar, leftMove)
      } yield calculateScore(leftMove, rightMove)
  }


  sealed trait Move {
    def score: Int
    def beats: Move
    def losesTo: Move
  }

  object Move {


    object Rock extends Move {
      override def score: Int = 1

      override def beats: Move = Scissors

      override def losesTo: Move = Paper
    }

    object Paper extends Move {
      override def score: Int = 2

      override def beats: Move = Rock

      override def losesTo: Move = Scissors
    }

    object Scissors extends Move {
      override def score: Int = 3

      override def beats: Move = Paper

      override def losesTo: Move = Rock
    }
  }
}
