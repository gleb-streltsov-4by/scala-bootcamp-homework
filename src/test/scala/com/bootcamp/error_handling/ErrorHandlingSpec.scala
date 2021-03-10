package com.bootcamp.error_handling

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import com.bootcamp.error_handling.ErrorHandling.PaymentCardValidator._
import cats.syntax.all._
import com.bootcamp.error_handling.ErrorHandling.ValidationError.{InvalidCardNumberFormat, InvalidExpirationDateFormat, InvalidOwnerFormat, InvalidSecurityCodeFormat}

class ErrorHandlingSpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  "validate" should "return valid payment card" in {
    val name = "Hleb Straltsou"
    val cardNumber = "1234567890123456"
    val expirationDate = "02/25"
    val securityCode = "998"

    validate(name, cardNumber, expirationDate, securityCode) shouldBe a [Valid[_]]
  }

  "validate" should "fail because of name and card number format" in {
    val name = "Invalid 2"
    val cardNumber = "123456780123456"
    val expirationDate = "02/25"
    val securityCode = "998"

    val actual = validate(name, cardNumber, expirationDate, securityCode)

    actual.leftMap(_.toList) shouldBe Invalid(
      List(InvalidOwnerFormat, InvalidCardNumberFormat)
    )
  }

  "validate" should "fail because of card number" in {
    val name = "Hleb Straltsou"
    val cardNumber = "123456780123456"
    val expirationDate = "02/25"
    val securityCode = "998"

    val actual = validate(name, cardNumber, expirationDate, securityCode)

    actual.leftMap(_.toList) shouldBe Invalid(
      List(InvalidCardNumberFormat)
    )
  }

  "validate" should "fail because of expiration date format" in {
    val name = "Hleb Straltsou"
    val cardNumber = "1234567810123456"
    val expirationDate = "02/255"
    val securityCode = "998"

    val actual = validate(name, cardNumber, expirationDate, securityCode)

    actual.leftMap(_.toList) shouldBe Invalid(
      List(InvalidExpirationDateFormat)
    )
  }

  "validate" should "fail because of expiration date and card number" in {
    val name = "Hleb"
    val cardNumber = "123456780123456"
    val expirationDate = "02/251"
    val securityCode = "998"

    val actual = validate(name, cardNumber, expirationDate, securityCode)

    actual.leftMap(_.toList) shouldBe Invalid(
      List(InvalidCardNumberFormat, InvalidExpirationDateFormat)
    )
  }

  "validate" should "fail because of security code format" in {
    val name = "Hleb Straltsou"
    val cardNumber = "1234567810123456"
    val expirationDate = "02/25"
    val securityCode = "9981"

    val actual = validate(name, cardNumber, expirationDate, securityCode)

    actual.leftMap(_.toList) shouldBe Invalid(
      List(InvalidSecurityCodeFormat)
    )
  }
}
