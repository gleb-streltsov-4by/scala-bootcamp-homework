package com.bootcamp.calculator

import com.bootcamp.calculator.ConsoleCalculator.Command._

import scala.language.implicitConversions

object ConsoleCalculator {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Option[Double], divisor: Option[Double]) extends Command
    final case class Sum(numbers: List[Option[Double]]) extends Command
    final case class Average(numbers: List[Option[Double]]) extends Command
    final case class Min(numbers: List[Option[Double]]) extends Command
    final case class Max(numbers: List[Option[Double]]) extends Command
  }

  final case class ErrorMessage(value: String) {
    def message: String = s"Error: $value"
  }

  final case class Result(command: Command, numbers: List[Double], result: Double)

  implicit def toDouble(s: Any): Option[Double] = s.toString.toDoubleOption
  implicit def toDouble(ss: List[Any]): List[Option[Double]] = ss.map(_.toString.toDoubleOption)

  def parseCommand(input: String): Either[ErrorMessage, Command] = {
    input.split("\\s+").toList match {
      case "divide" :: dividend :: divisor :: Nil => Right(Divide(dividend, divisor))

      case "sum"      :: numbers  => Right(Sum(numbers))
      case "average"  :: numbers  => Right(Average(numbers))
      case "min"      :: numbers  => Right(Min(numbers))
      case "max"      :: numbers  => Right(Max(numbers))

      case _ => Left(ErrorMessage("parse command ..."))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = {

    def handle(c: Command, ns: List[Option[Double]])(
      f: List[Double] => Double): Either[ErrorMessage, Result] = {

      if (ns.forall(_.isDefined)) Right(Result(c, ns.flatten, f(ns.flatten)))
      else Left(ErrorMessage("invalid arguments ..."))
    }

    command match {
      case Divide(_, Some(0))       => Left(ErrorMessage("division by zero ..."))
      case Divide(Some(a), Some(b)) => Right(Result(command, List(a, b), a / b))

      case Sum(nums)      => handle(command, nums)(ls => ls.sum)
      case Average(nums)  => handle(command, nums)(ls => ls.sum / ls.length)
      case Min(nums)      => handle(command, nums)(ls => ls.min)
      case Max(nums)      => handle(command, nums)(ls => ls.max)

      case _              => Left(ErrorMessage(s"calculating with $command ..."))
    }
  }

  def renderResult(r: Result): String = {

    def obtain(d: Double): String =
      if (d.toLong == d) d.toLong.toString
      else d.toString

    def obtainList(ls: List[Double]): String = ls.map(d => obtain(d)).mkString(" ")

    r.command match {
      case Divide(Some(a), Some(b)) => s"${obtain(a)} divided by ${obtain(b)} is ${r.result}"

      case Sum(_)       => s"the sum of ${obtainList(r.numbers)} is ${obtain(r.result)}"
      case Average(_)   => s"the average of ${obtainList(r.numbers)} is ${obtain(r.result)}"
      case Min(_)       => s"the minimum of ${obtainList(r.numbers)} is ${obtain(r.result)}"
      case Max(_)       => s"the maximum of ${obtainList(r.numbers)} is ${obtain(r.result)}"

      case _            => "Error: render result ..."
    }
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result  <- calculate(command)
    } yield result) match {
      case Left(error)    => error.message
      case Right(result)  => renderResult(result)
    }
  }
}
