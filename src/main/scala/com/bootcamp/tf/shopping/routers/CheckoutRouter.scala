package com.bootcamp.tf.shopping.routers

import cats.Monad
import cats.data.{Kleisli, OptionT}
import cats.syntax.all._
import com.bootcamp.tf.shopping.domain.card.Card
import com.bootcamp.tf.shopping.domain.user.UserId
import com.bootcamp.tf.shopping.effects.{CurrencySupport, GenUUID, ToNumeric}
import com.bootcamp.tf.shopping.services.CheckoutService

object CheckoutRouter {

  def apply[F[_]: Monad: GenUUID: ToNumeric: CurrencySupport](
    checkoutService: CheckoutService[F]
  ): Kleisli[OptionT[F, *], List[String], String] = Kleisli[OptionT[F, *], List[String], String] {
    case userId :: cardNumber :: cvv :: _ =>
      OptionT.liftF {
        for {
          userId <- GenUUID[F].read(userId)
          result <- checkoutService.checkout(UserId(userId), Card(cardNumber, cvv))
        } yield result.toString
      }

    case _ => OptionT.none
  }

}
