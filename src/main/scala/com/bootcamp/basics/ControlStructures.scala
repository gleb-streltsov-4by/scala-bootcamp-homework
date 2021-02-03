package com.bootcamp.basics

import scala.io.Source

object ControlStructures {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    ???
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    ???
  }

  def renderResult(x: Result): String = {
    ???
  }

  def process(x: String): String = {
    import cats.implicits._

    ???
  }

  def main(args: Array[String]): Unit = {
    for {
      str <- Source.stdin.getLines().map(line =>
        process(line)
      )
    } println(str)
  }
}
