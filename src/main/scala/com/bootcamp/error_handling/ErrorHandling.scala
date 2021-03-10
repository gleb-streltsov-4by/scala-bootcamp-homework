package com.bootcamp.error_handling

import cats.data.{Validated, ValidatedNec}
import cats.implicits.catsSyntaxTuple4Semigroupal
import eu.timepit.refined.string.{MatchesRegex, Trimmed}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import cats.syntax.all._

import java.time.YearMonth
import java.time.format.DateTimeFormatter
import scala.util.Try

object ErrorHandling {

  type Owner = String Refined MatchesRegex[W.`"^(([A-Za-z]+ ?){1,3})$"`.T]
  type CardNumber = String Refined MatchesRegex[W.`"^[0-9]{16}$"`.T]
  type SecurityCode = String Refined MatchesRegex[W.`"^[0-9]{3}$"`.T]
  type ExpirationDate = YearMonth

  val expirationDateFormat: String = "MM/yy"

  final case class PaymentCard(owner: Owner, number: CardNumber, expirationDate: ExpirationDate, securityCode: SecurityCode)

  sealed trait ValidationError
  object ValidationError {
    final case object InvalidOwnerFormat extends ValidationError {
      def message: String = "Invalid owner: should contain alphabetic first name and last name in format `XXX YYY`"
    }
    final case object InvalidCardNumberFormat extends ValidationError {
      def message: String = "Invalid card number format: should contain 16 digits in format `XXXXXXXXXXXXXXXX`"
    }
    final case object InvalidExpirationDateFormat extends ValidationError {
      def message: String = "Invalid expiration date format: should contain valid month and year in format mm/yy"
    }
    final case object PaymentCardExpiration extends ValidationError {
      def message: String = "Payment card is expired"
    }
    final case object InvalidSecurityCodeFormat extends ValidationError {
      def message: String = "Invalid security code format: should contain 3 digits"
    }
  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(name: String,
                 cardNumber: String,
                 expirationDate: String,
                 securityCode: String): AllErrorsOr[PaymentCard] = {
      (validateOwner(name),
        validateCardNumber(cardNumber),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)).mapN(PaymentCard)
    }

    private def validateOwner(name: String): AllErrorsOr[Owner] = {
      val r: Either[String, Owner] = refineV(name)
      Validated.fromEither(r.leftMap(_ => InvalidOwnerFormat)).toValidatedNec
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {
      val r: Either[String, CardNumber] = refineV(number)
      Validated.fromEither(r.leftMap(_ => InvalidCardNumberFormat)).toValidatedNec
    }

    private def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] = {
      val r: Either[String, SecurityCode] = refineV(code)
      Validated.fromEither(r.leftMap(_ => InvalidSecurityCodeFormat)).toValidatedNec
    }

    private def validateExpirationDate(rawDate: String): AllErrorsOr[ExpirationDate] = {

      def validateFormat(rawDate: String): Either[ValidationError, ExpirationDate] =
        Try(YearMonth.parse(rawDate, DateTimeFormatter.ofPattern(expirationDateFormat)))
            .toEither
            .leftMap(_ => InvalidExpirationDateFormat)

      def validateExpiration(validDate: ExpirationDate): Either[ValidationError, ExpirationDate] =
        Either.cond(validDate.isAfter(YearMonth.now()), validDate, PaymentCardExpiration)

      val result = for {
        validDate   <- validateFormat(rawDate)
        _           <- validateExpiration(validDate)
      } yield validDate

      Validated.fromEither(result ).toValidatedNec
    }
  }
}
