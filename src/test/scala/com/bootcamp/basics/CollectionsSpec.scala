package com.bootcamp.basics

import com.bootcamp.basics.Collections._
import com.bootcamp.basics.LinkedList._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class CollectionsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "runningSum" should "return correct results" in {
    val input = List(1,2,3,4)
    val output = List(1,3,6,10)

    runningSum(input) shouldEqual output
  }

  "runningSum" should "return correct result with empty list" in {
    val input = List()
    val output = List()

    runningSum(input) shouldEqual output
  }

//  "shuffle" should "return correct results" in {
//    val input = List(1,2,3,4,5,6)
//    val output = List(1,4,2,5,3,6)
//
//    shuffle(input) shouldEqual output
//  }

  "shuffle" should "return correct result with empty list" in {
    val input = List()
    val output = List()

    shuffle(input) shouldEqual output
  }

  "richest" should "return correct results #1" in {
    val input = List(List(1,2,3), List(3,2,1))
    val output = Some(6)

    richest(input) shouldEqual output
  }

  "richest" should "return correct results #2" in {
    val input = List(List(1,2,5),List(7,2,3))
    val output = Some(12)

    richest(input) shouldEqual output
  }

  "richest" should "return correct result with empty matrix" in {
    val input1 = List()
    val input2 = List(List(), List())
    val output = None

    richest(input1) shouldEqual output
    richest(input2) shouldEqual output
  }

  "findGap" should "find gap" in {
    findGap(List(1, 2, 3, 5, 6)) shouldEqual Some(3, 5)
  }

  "findGap" should "work correctly on empty" in {
    findGap(List.empty) shouldEqual None
  }

  "findGap" should "work correctly on no gaps" in {
    findGap((1 to 100).toList) shouldEqual None
  }

  "min" should "work correctly on empty" in {
    min(Nil) shouldEqual None
  }

  "min" should "work correctly on non empty" in {
    min(Random.shuffle(1 to 100).toList) shouldEqual Some(1)
  }

  "scanLeft" should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
    println(scanLeft(0)((1 to 5).toList)(_ + _))
  }

  "scanLeft" should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
    println(scanLeft("")(('a' to 'd').toList.map(_.toString))(_ + _))
  }
//
//  "count" should "pass" in {
//    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
//  }
}
