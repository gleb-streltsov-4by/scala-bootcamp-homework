package com.bootcamp.crud.book.servers

import cats.implicits._
import cats.effect.{ConcurrentEffect, Timer}
import com.bootcamp.crud.book.routes.BookRoutes._
import com.bootcamp.crud.book.service.BookService
import fs2.Stream
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._

import scala.concurrent.ExecutionContext.global

object BookServer {

  val host = "localhost"
  val port = 9000

  def stream[F[_] : ConcurrentEffect](implicit T: Timer[F]): Stream[F, Nothing] = {
    val bookService = BookService.impl[F]

    val httpApp = (
      getRoutes[F](bookService) <+>
      putRoutes[F](bookService) <+>
      postRoutes[F](bookService) <+>
      deleteRoutes[F](bookService)).orNotFound

    for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(port, host)
        .withHttpApp(httpApp)
        .serve
    } yield exitCode

  }.drain
}
