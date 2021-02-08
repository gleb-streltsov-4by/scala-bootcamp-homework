package com.bootcamp.collections

import scala.annotation.tailrec

object Collections {

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle[T](ls: List[T]): List[T] = ls match {
    case Nil => Nil
    case _ =>
      val len = ls.length / 2
      (ls.take(len) zip ls.takeRight(len))
        .flatMap { case (x, y) => List(x, y) }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def richest(matrix: List[List[Int]]): Option[Int] = {
    def maxWithOpt(a: Int, opt: Option[Int]): Option[Int] = {
      (a, opt) match {
        case (a, Some(b)) => Some(a max b)
        case (a, _) => Some(a)
      }
    }

    @tailrec
    def helper(acc: Option[Int], matrix: List[List[Int]]): Option[Int] = matrix match {
      case Nil => acc
      case h :: t if h.nonEmpty => helper(maxWithOpt(h.sum, acc), t)
      case _ => helper(acc, matrix.tail)
    }
    helper(None, matrix)
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def greatest(candies: List[Int], extraCandies: Int): List[Boolean] = candies match {
    case Nil  => Nil
    case _    =>
      val max = candies.max
      candies.map(c => c + extraCandies >= max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points/
  def widest(points: List[(Int, Int)]): Option[Int] = {
    @tailrec
    def helper(acc: Option[Int], xs: List[Int]): Option[Int] = xs match {
      case a :: b :: t => helper(
        if (b - a > acc.getOrElse(Int.MinValue)) Some(b - a) else acc,
        b :: t)
      case _ => acc
    }

    points match {
      case Nil  => None
      case _    =>
        val xs = points
          .map { case (x, _) => x }
          .sorted

        helper(None, xs) match {
          case Some(0)  => None
          case v        => v
        }
    }
  }

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case h :: t => t.scanLeft(h)(_ + _)
  }
}

object LinkedList extends App {

  @tailrec
  def findGap(ls: List[Int]): Option[(Int, Int)] = ls match {
    case Nil => None
    case h :: t if t.nonEmpty =>
      if (t.head - h > 1) Some((h, t.head))
      else findGap(t)
    case _ => None
  }

  // try to implement min different ways (fold, reduce, recursion)
  // def min(ls: List[Int]): Option[Int] = ls match {
  def min(ls: List[Int]): Option[Int] = {
//    case Nil => None
//    case h :: _ => Some(ls.foldLeft(h)(_ min _))

//    case Nil => None
//    case _ => Some(ls.reduce(_ min _))

//    ls.minOption

    @tailrec
    def helper(ls: List[Int], acc: Int): Option[Int] = ls match {
      case Nil => Some(acc)
      case h :: t => helper(t, h min acc)
    }

    ls match {
      case Nil => None
      case h :: t => helper(t, h)
    }
  }

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(ls: List[T])(f: (T, T) => T): List[T] = {
    ls.foldLeft(List(zero))((acc, e) => f(acc.head, e) :: acc).reverse
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = s match {
    case _ if s.isEmpty => Nil
    case _ =>
      val prefix = s.takeWhile(c => c == s.head)
      s.head -> prefix.length :: count(s.drop(prefix.length))
  }
}
