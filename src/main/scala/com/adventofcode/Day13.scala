package com.adventofcode

import com.adventofcode.Common.CharHelper

import scala.annotation.tailrec

object Day13 {

  sealed trait Packet

  final case class Digit(value: Int) extends Packet
  final case class Nested(values: List[Packet] = List.empty) extends Packet {
    def add(packet: Packet): Nested =
      this.copy(values = values :+ packet)

    def close: Nested =
      this.copy(values = values :+ Digit(-1))
  }

  final case class Packets(left: Packet, right: Packet)

  case class PacketState(
      allNested: List[Nested] = List.empty,
      curNested: Nested = Nested(),
      curDigit: Option[Int] = None,
      res: Option[Nested] = None
  ) {
    def closeNested: PacketState = {
      val newNested = curDigit
        .map(value => curNested.add(Digit(value)))
        .getOrElse(curNested)
        .close

      allNested.lastOption match {
        case Some(value) =>
          this.copy(
            allNested = allNested.dropRight(1),
            curNested = value.add(newNested),
            curDigit = None
          )
        case None =>
          this.copy(res = Some(newNested))
      }
    }

    def openNested: PacketState =
      this.copy(
        allNested = allNested :+ curNested,
        curNested = Nested()
      )

    def closeDigit: PacketState = curDigit match {
      case Some(value) =>
        this.copy(curNested = curNested.add(Digit(value)), curDigit = None)
      case _ => this
    }

    def updateDigit(value: Int): PacketState =
      this.copy(curDigit = Some((curDigit.getOrElse(0) * 10) + value))
  }

  def readPacket(line: String): Option[Packet] = {
    @tailrec
    def readPacket0(curLine: String, state: PacketState): PacketState =
      curLine.headOption match {
        case Some('[') => readPacket0(curLine.drop(1), state.openNested)
        case Some(']') => readPacket0(curLine.drop(1), state.closeNested)
        case Some(',') => readPacket0(curLine.drop(1), state.closeDigit)
        case Some(n) =>
          n.toIntOption match {
            case Some(value) =>
              readPacket0(curLine.drop(1), state.updateDigit(value))
            case _ => state
          }
        case _ => state
      }

    val r = readPacket0(line.drop(1), PacketState())
    r.res
  }

  def rightOrder(leftPacket: Packet, rightPacket: Packet): Boolean = {
    @tailrec
    def rightOrder0(left: List[Packet], right: List[Packet]): Boolean =
      (left.headOption, right.headOption) match {
        case (Some(leftValue), Some(rightValue)) =>
          (leftValue, rightValue) match {
            case (Digit(leftDigit), Digit(rightDigit)) =>
              if (leftDigit < rightDigit) true
              else if (leftDigit > rightDigit) false
              else rightOrder0(left.drop(1), right.drop(1))
            case (_: Nested, rightDigit: Digit) =>
              rightOrder0(
                left,
                Nested(List(rightDigit)).close +: right.drop(1)
              )
            case (leftDigit: Digit, _: Nested) =>
              rightOrder0(
                Nested(List(leftDigit)).close +: left.drop(1),
                right
              )
            case (Nested(leftValues), Nested(rightValues)) =>
              rightOrder0(
                leftValues ++ left.drop(1),
                rightValues ++ right.drop(1)
              )
          }
        case (_, None) => false
        case (None, _) => true
        case _         => false
      }

    rightOrder0(List(leftPacket), List(rightPacket))
  }

  def readGroup(lines: List[String]): Option[Packets] =
    for {
      left <- lines.headOption.flatMap(readPacket)
      right <- lines.lift(1).flatMap(readPacket)
    } yield Packets(left, right)

  def readPackets(lines: List[String]): List[Packets] =
    lines.grouped(3).flatMap(readGroup).toList

  object Task1 {
    def solve(allPackets: List[Packets]): Int =
      allPackets.zipWithIndex
        .filter { case (packets, _) => rightOrder(packets.left, packets.right) }
        .map(_._2 + 1)
        .sum
  }

  object Task2 {
    def solve(allPackets: List[Packets]): Int = {
      val dividers = List(
        Nested().add(Nested().add(Digit(2)).close).close,
        Nested().add(Nested().add(Digit(6)).close).close
      )

      val unsortedPackets = allPackets.flatMap(packets =>
        List(packets.left, packets.right)
      ) ++ dividers

      unsortedPackets
        .sortWith(rightOrder)
        .zipWithIndex
        .filter { case (packet, _) => dividers.contains(packet) }
        .map(_._2 + 1)
        .product
    }
  }

  def process(fun: List[Packets] => Int): Option[Int] =
    Common
      .readFile("src/main/resources/day13/task.txt", readPackets)
      .map(fun)

  def main(args: Array[String]): Unit = {
    println(process(Task1.solve))
    println(process(Task2.solve))
  }
}
