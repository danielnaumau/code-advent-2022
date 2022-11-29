package com.adventofcode

import scala.io.Source
import scala.util.Using

object Common {
  def readFile[T](path: String, process: List[String] => T): Option[T] =
    Using(Source.fromFile(path))(file => process(file.getLines().toList)).toOption
}
