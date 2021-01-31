package com.bootcamp.basics

import scala.annotation.tailrec

object Basics {

  def lcm(a: Int, b: Int): Int =
    Math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) Math.abs(a)
    else gcd(b, a % b)
}
