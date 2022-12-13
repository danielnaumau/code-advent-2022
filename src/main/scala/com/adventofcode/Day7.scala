package com.adventofcode

import com.adventofcode.Common.MapHelper

object Day7 {
  private val TOTAL_SIZE = 70000000
  private val REQUIRED_SPACE = 30000000

  final case class State(
      directorySizes: Map[String, Long],
      curDirectories: List[String],
      usedFiles: Set[String]
  ) {
    def goToRoot(): State =
      this.copy(
        directorySizes = directorySizes.addOrUpdate("/", 0L),
        curDirectories = List("/")
      )

    def goBack(): State =
      this.copy(curDirectories = curDirectories.dropRight(1))

    def cd(dirName: String): State = {
      val dirNameFull = s"${curDirectories.lastOption.getOrElse("/")}/$dirName"
      this.copy(
        directorySizes = directorySizes.addOrUpdate(dirNameFull, 0L),
        curDirectories = curDirectories :+ dirNameFull
      )
    }

    def addFile(fileName: String, size: Int): State = {
      val fileNameFull = (curDirectories :+ fileName).mkString("/")
      if (usedFiles.contains(fileNameFull)) {
        this
      } else {
        this.copy(
          directorySizes = curDirectories.foldLeft(directorySizes) {
            case (curState, dirName) => curState.addOrUpdate(dirName, size)
          },
          usedFiles = usedFiles + fileNameFull
        )
      }
    }
  }

  object State {
    def empty: State = State(Map.empty, List.empty, Set.empty)
  }

  object Task1 {
    def solve(directorySizes: List[Long]): Long =
      directorySizes.filter(_ < 100000).sum
  }

  object Task2 {
    def solve(directorySizes: List[Long]): Long = {
      val sortedSizes = directorySizes.sorted
      val freeSpace =
        REQUIRED_SPACE - (TOTAL_SIZE - sortedSizes.lastOption.getOrElse(0L))
      sortedSizes.filter(_ >= freeSpace).minOption.getOrElse(0L)
    }
  }

  def main(args: Array[String]): Unit = {
    println(process(Task1.solve))
    println(process(Task2.solve))
  }

  def process(solve: List[Long] => Long): Option[Long] =
    Common
      .readFile("src/main/resources/day7/task.txt", findDirectorySizes)
      .map(solve)

  def findDirectorySizes(lines: List[String]): List[Long] =
    lines
      .foldLeft(State.empty)(processCommand)
      .directorySizes
      .values
      .toList

  def processCommand(state: State, command: String): State =
    command.replace("$", "&") match {
      case s"& cd /"        => state.goToRoot()
      case s"& cd .."       => state.goBack()
      case s"& cd $dirName" => state.cd(dirName)
      case s"$size $fileName" =>
        size.toIntOption.map(state.addFile(fileName, _)).getOrElse(state)
      case _ => state
    }

}
