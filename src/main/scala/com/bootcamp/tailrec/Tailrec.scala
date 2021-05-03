package com.bootcamp.tailrec

import scala.annotation.tailrec

object Tailrec {

  def fibonacci(n: Int): Int = {

    @tailrec
    def helper(n: Int, cur: Int, prev: Int): Int = n match {
      case 0 => cur
      case _ => helper(n - 1, cur + prev, cur)
    }

    helper(n, cur = 1, prev = 0)
  }

  def fibonacciEvenSum(n: Int): Int = {

    @tailrec
    def helper(n: Int, acc: Int, cur: Int, prev: Int): Int = {
      if (n <= 1) acc
      else if (n % 2 == 0) helper(n - 1, acc, cur + prev, cur)
      else helper(n - 1, acc + cur, cur + prev, cur)
    }

    helper(n, acc = 1, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    println(fibonacci(0))
    println(fibonacci(1))
    println(fibonacci(2))
    println(fibonacci(3))
    println(fibonacci(4))
    println(fibonacci(5))
    println(fibonacci(6))
    println(fibonacci(7))
    println("-----------")
    println(fibonacciEvenSum(1))
    println(fibonacciEvenSum(2))
    println(fibonacciEvenSum(3))
    println(fibonacciEvenSum(4))
    println(fibonacciEvenSum(5))
    println(fibonacciEvenSum(6))
    println(fibonacciEvenSum(7))
  }
}
