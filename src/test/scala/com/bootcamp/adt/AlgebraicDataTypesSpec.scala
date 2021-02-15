package com.bootcamp.adt

import com.bootcamp.adt.AlgebraicDataTypes.{Card, PokerError}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AlgebraicDataTypesSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "Card" should "correctly created" in {
    val input = "Ad"
    val output = Card.from(input)

    output.isRight shouldEqual true
  }

  "Card" should "return invalid card error with too long input" in {
    val input = "Addfds"
    val output = PokerError("Illegal argument for `Card`", input)

    Card.from(input) shouldEqual Left(output)
  }

  "Card" should "return invalid card error with empty input" in {
    val input = ""
    val output = PokerError("Illegal argument for `Card`", input)

    Card.from(input) shouldEqual Left(output)
  }

  "Card" should "return invalid rank error" in {
    val input = "Pd"
    val output = PokerError("Illegal argument for `Rank`", "P")

    Card.from(input) shouldEqual Left(output)
  }

  "Card" should "return invalid suit error" in {
    val input = "Ao"
    val output = PokerError("Illegal argument for `Suit`", "o")

    Card.from(input) shouldEqual Left(output)
  }
}
