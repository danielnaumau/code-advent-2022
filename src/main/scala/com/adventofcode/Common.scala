package com.adventofcode

import scala.io.Source
import scala.util.Using

object Common {
  def readFile[T](path: String, convert: List[String] => T): Option[T] =
    Using(Source.fromFile(path))(file => convert(file.getLines().toList)).toOption

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
}
