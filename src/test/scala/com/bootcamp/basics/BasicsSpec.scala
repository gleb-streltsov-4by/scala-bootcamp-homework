package com.bootcamp.basics

import com.bootcamp.basics.Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BasicsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "gcd" should "be equal to 1 with simple numbers" in {
    gcd(13, 3) shouldEqual 1
  }

  "gcd" should "be independent from order of numbers" in {
    gcd(12, 16) shouldEqual 4
    gcd(16, 12) shouldEqual 4
  }

  "gcd" should "be equal to the greater number if zero present" in {
    gcd(0, 4) shouldEqual 4
    gcd(5, 0) shouldEqual 5
  }

  "gcd" should "be equal to 0 in case `a` and `b` are equal to 0" in {
    gcd(0, 0) shouldEqual 0
  }

  "gcd" should "return correct results with positive numbers" in {
    gcd(21, 14) shouldEqual 7
    gcd(27, 36) shouldEqual 9
    gcd(2, 3) shouldEqual 1
  }

  "gcd" should "return correct results with negative numbers" in {
    gcd(-21, -14) shouldEqual 7
    gcd(-27, -36) shouldEqual 9
    gcd(-2, -3) shouldEqual 1
  }

  "gcd" should "return correct results with negative and positive numbers" in {
    gcd(-21, 14) shouldEqual 7
    gcd(27, -36) shouldEqual 9
    gcd(2, -3) shouldEqual 1
    gcd(0, -2) shouldEqual 2
  }

  "lcm" should "be equal to product with simple numbers" in {
    lcm(13, 3) shouldEqual Right(39)
  }

  "lcm" should "be independent from order of numbers" in {
    lcm(18, 36) shouldEqual Right(36)
    lcm(36, 18) shouldEqual Right(36)
  }

  "lcm" should "be equal to the greater number if zero present" in {
    lcm(0, 4) shouldEqual Right(0)
    lcm(5, 0) shouldEqual Right(0)
  }

  "lcm" should "return correct results with positive numbers" in {
    lcm(4, 5) shouldEqual Right(20)
    lcm(14, 21) shouldEqual Right(42)
    lcm(1, 1) shouldEqual Right(1)
  }

  "lcm" should "return correct results with negative numbers" in {
    lcm(-4, -5) shouldEqual Right(20)
    lcm(-14, -21) shouldEqual Right(42)
    lcm(-1, -1) shouldEqual Right(1)
  }

  "lcm" should "return correct results with negative and positive numbers" in {
    lcm(4, -5) shouldEqual Right(20)
    lcm(-14, 21) shouldEqual Right(42)
    lcm(1, -1) shouldEqual Right(1)
  }

  "lcm" should "be equal to Either in case `a` and `b` are equal to 0" in {
    lcm(0, 0) shouldEqual Left("`a` and `b` cannot be simultaneously equal to 0")
  }
}
