package com.bootcamp.basics

import scala.annotation.tailrec

object Basics {

  def lcm(a: Int, b: Int): Either[String, Int] =
    if (a == 0 && b == 0) Left(s"`a` and `b` cannot be simultaneously equal to 0")
    else Right(Math.abs(a * b) / gcd(a, b))

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) Math.abs(a)
    else gcd(b, a % b)
}
