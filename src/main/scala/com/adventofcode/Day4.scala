package com.adventofcode

object Day4 {

  final case class Section(start: Int, end: Int) {
    def contains(section: Section): Boolean =
      section.start >= start && section.end <= end

    def overlaps(section: Section): Boolean =
      (section.start >= start && section.start <= end) || (section.end >= start && section.end <= end)
  }

  def read(sectionStr: String): Option[Section] =
    sectionStr.split('-').toList match {
      case startStr :: endStr :: Nil =>
        for {
          start <- startStr.toIntOption
          end <- endStr.toIntOption
        } yield Section(start, end)
      case _ => None
    }

  def main(args: Array[String]): Unit = {
    println(solve(Task1.contain))
    println(solve(Task2.overlap))
  }

  def solve(condition: (Section, Section) => Boolean): Option[Int] =
    Common.readFile("src/main/resources/day4/crates.txt", convert(condition))

  object Task1 {
    def contain(leftSection: Section, rightSection: Section): Boolean =
      leftSection.contains(rightSection) || rightSection.contains(leftSection)
  }

  object Task2 {
    def overlap(leftSection: Section, rightSection: Section): Boolean =
      leftSection.overlaps(rightSection) || rightSection.overlaps(leftSection)
  }

  def convert(
      condition: (Section, Section) => Boolean
  )(lines: List[String]): Int =
    lines.count(_.split(',').toList match {
      case leftSectionStr :: rightSectionStr :: Nil =>
        val res = for {
          leftSection <- read(leftSectionStr)
          rightSection <- read(rightSectionStr)
        } yield condition(leftSection, rightSection)
        res.getOrElse(false)
      case _ => false
    })
}
