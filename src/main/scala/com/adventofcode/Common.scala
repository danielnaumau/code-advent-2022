package com.adventofcode

import scala.io.Source
import scala.util.Using

object Common {
  implicit class MapHelper(map: Map[String, Long]) {
    def addOrUpdate(key: String, value: Long): Map[String, Long] = {
      map + (key -> (map.getOrElse(key, 0L) + value))
    }
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
  }

  object Matrix {
    def fill[T](height: Int, width: Int)(elem: T): Matrix[T] =
      new Matrix[T](List.fill(width)(List.fill(height)(elem)))

    def empty[T](width: Int): Matrix[T] =
      new Matrix[T](List.fill(width)(List()))
  }
}
