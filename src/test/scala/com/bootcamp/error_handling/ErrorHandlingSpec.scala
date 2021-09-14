package com.bootcamp.error_handling

import cats.data.Chain
import com.bootcamp.error_handling.ErrorHandling.AccountValidationError.InvalidSecurityCode
import com.bootcamp.error_handling.ErrorHandling.AccountValidator.validateSecurityCode
import org.scalatest.freespec.AnyFreeSpec

class ErrorHandlingSpec extends AnyFreeSpec {

  "valid security code: 444" in {
    val inputSecurityCode = "444"
    val expected          = "444"

    val actual = validateSecurityCode(inputSecurityCode)

    assert(actual.isValid)

    actual.fold(
      e => fail(s"Doesn't expect an error: $e"),
      v => assert(v.value == expected)
    )
  }

  "invalid security code: 4444" in {
    val inputSecurityCode = "4444x"
    val expected          = Chain(InvalidSecurityCode)

    val actual = validateSecurityCode(inputSecurityCode)

    assert(actual.isInvalid)

    actual.fold(
      e => assert(e === expected),
      v => fail(s"Doesn't expect a Valid value: $v")
    )
  }
}
