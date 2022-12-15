package com.adventofcode

import scala.io.Source
import scala.util.Using

object Common {
  implicit class MapHelper(map: Map[String, Long]) {
    def addOrUpdate(key: String, value: Long): Map[String, Long] =
      map + (key -> (map.getOrElse(key, 0L) + value))
  }

  implicit class ListHelper[A](l: List[Option[A]]) {
    def sequence: Option[List[A]] =
      l.foldLeft[Option[List[A]]](Some(List.empty[A])) {
        case (Some(list), Some(cur)) => Some(list :+ cur)
        case (_, _)                  => None
      }
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
  }

  final case class Matrix[T](values: List[List[T]]) {
    def addRow(row: List[T]): Matrix[T] =
      Matrix(
        values
          .zip(row)
          .map { case (allValues, newRow) =>
            allValues :+ newRow
          }
      )

    def height: Int = values.headOption.map(_.length).getOrElse(0)

    def width: Int = values.length

    def getColumn(index: Int): Option[List[T]] =
      values.lift(index)

    def get(rowIndex: Int, columnIndex: Int): Option[T] =
      values.lift(columnIndex).flatMap(_.lift(rowIndex))

    def getRow(index: Int): Option[List[T]] =
      values.map(_.lift(index)).sequence
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
