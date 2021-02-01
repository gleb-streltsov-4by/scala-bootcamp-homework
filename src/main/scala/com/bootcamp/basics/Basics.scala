package com.bootcamp.basics

import cats.implicits.catsSyntaxOptionId

import scala.annotation.tailrec

object Basics {

  def lcm(a: Int, b: Int): Option[Int] =
    if (a == 0 && b == 0) None
    else (Math.abs(a * b) / gcd(a, b)).some

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) Math.abs(a)
    else gcd(b, a % b)
}
