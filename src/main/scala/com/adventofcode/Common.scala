package com.adventofcode

import scala.io.Source
import scala.util.Using

object Common {
  implicit class IndexHelper[A](l: List[A]) {
    def indexWhereOpt(fun: A => Boolean): Option[Int] =
      l.indexWhere(fun) match {
        case -1    => None
        case value => Some(value)
      }
  }

  implicit class MapHelper(map: Map[String, Long]) {
    def addOrUpdate(key: String, value: Long): Map[String, Long] =
      map + (key -> (map.getOrElse(key, 0L) + value))
  }

  implicit class ListSequence[A](l: List[Option[A]]) {
    def sequence: Option[List[A]] =
      l.foldLeft[Option[List[A]]](Some(List.empty[A])) {
        case (Some(list), Some(cur)) => Some(list :+ cur)
        case (_, _)                  => None
      }
  }

  implicit class ListTraverse[A](l: List[A]) {
    def traverse[B](fun: A => Option[B]): Option[List[B]] =
      l.map(fun).sequence
  }

  implicit class ListUpdatedWith[A](l: List[A]) {
    def updatedWith(index: Int, fun: A => A): List[A] =
      l.lift(index) match {
        case Some(value) => l.updated(index, fun(value))
        case None        => l
      }
  }

  implicit class MatrixCharHelper(m: Matrix[Char]) {
    def image: String =
      (0 until m.height).toList
        .flatMap(y => m.getRow(y).map(_.mkString))
        .mkString("\n")
  }

  final case class Coordinate(x: Int, y: Int)

  implicit class CharHelper(char: Char) {
    def toIntOption: Option[Int] =
      char.toString.toIntOption
  }

  def readFile[T](path: String, convert: List[String] => T): Option[T] =
    Using(Source.fromFile(path))(file =>
      convert(file.getLines().toList)
    ).toOption

  final case class NonEmptyList[A](head: A, tail: List[A] = Nil) {
    def map[V](fun: A => V): NonEmptyList[V] =
      NonEmptyList(fun(head), tail.map(fun))

    def last: A =
      tail.lastOption.getOrElse(head)

    def toList: List[A] = head +: tail

    def contains(value: A): Boolean =
      tail.contains(value) || value == head

    def exists(fun: A => Boolean): Boolean =
      tail.exists(fun) || fun(head)

    def +:(value: A): NonEmptyList[A] =
      this.copy(value, tail = head +: tail)

    def :+(value: A): NonEmptyList[A] =
      this.copy(tail = tail :+ value)

    def ++(values: List[A]): NonEmptyList[A] =
      this.copy(tail = tail ++ values)
  }

  final case class MatrixValue[T](value: T, coordinate: Coordinate)

  final case class Matrix[T](values: List[List[T]]) {
    def addRow(row: List[T]): Matrix[T] =
      Matrix(
        values
          .zip(row)
          .map { case (allValues, newRow) =>
            allValues :+ newRow
          }
      )

    def update(newValues: List[MatrixValue[T]]): Matrix[T] =
      newValues.foldLeft(this)(_.update(_))

    def update(newValue: MatrixValue[T]): Matrix[T] =
      Matrix(
        values
          .updatedWith(
            newValue.coordinate.x,
            _.updated(newValue.coordinate.y, newValue.value)
          )
      )

    def height: Int = values.headOption.map(_.length).getOrElse(0)

    def width: Int = values.length

    def getColumn(x: Int): Option[List[T]] =
      values.lift(x)

    def find(value: T): Option[MatrixValue[T]] =
      for {
        x <- values.indexWhereOpt(_.contains(value))
        y <- values.lift(x).flatMap(_.indexWhereOpt(_ == value))
      } yield MatrixValue(value, Coordinate(x, y))

    def findAll(value: T): List[MatrixValue[T]] =
      for {
        (column, x) <- values.zipWithIndex.filter(_._1.contains(value))
        y <- column.zipWithIndex.filter(_._1 == value).map(_._2)
      } yield MatrixValue(value, Coordinate(x, y))

    def neighbours(y: Int, x: Int): List[MatrixValue[T]] =
      List(
        getMatrixValue(x - 1, y),
        getMatrixValue(x, y - 1),
        getMatrixValue(x + 1, y),
        getMatrixValue(x, y + 1)
      ).flatten

    def getMatrixValue(x: Int, y: Int): Option[MatrixValue[T]] =
      get(y, x).map(MatrixValue(_, Coordinate(x, y)))

    def get(y: Int, x: Int): Option[T] =
      values.lift(x).flatMap(_.lift(y))

    def getRow(y: Int): Option[List[T]] =
      values.map(_.lift(y)).sequence
  }

  object NonEmptyList {
    def fill[A](tailSize: Int)(a: A): NonEmptyList[A] =
      NonEmptyList(a, List.fill(tailSize)(a))
  }

  object Matrix {
    def fill[T](height: Int, width: Int)(elem: T): Matrix[T] =
      new Matrix[T](List.fill(width)(List.fill(height)(elem)))

    def empty[T](width: Int): Matrix[T] =
      new Matrix[T](List.fill(width)(List()))
  }
}
