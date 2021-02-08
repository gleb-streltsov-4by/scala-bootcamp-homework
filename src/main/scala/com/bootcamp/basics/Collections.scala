package com.bootcamp.basics

import scala.annotation.tailrec

object Collections {

  def runningSum(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case h :: t => t.scanLeft(h)(_ + _)
  }

  def shuffle[T](ls: List[T]): List[T] = {
    ls match {
      case Nil => Nil
      case _ => Nil
    }
  }

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
  def count(s: String): List[(Char, Int)] = {
    ???
  }
}
