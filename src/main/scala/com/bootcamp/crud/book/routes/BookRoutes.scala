package com.bootcamp.crud.book.routes

import cats.implicits._
import cats.data.Validated
import cats.effect.Sync
import com.bootcamp.crud.book.service.BookService
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io._
import com.bootcamp.crud.book.domain.book._
import com.bootcamp.crud.book.dto.book._
import com.bootcamp.crud.book.service.error.book._
import com.bootcamp.crud.book.service.error.book.BookValidationError._

import java.util.UUID

object BookRoutes {

  private val domain = "book"

  implicit val uuidDecoder: QueryParamDecoder[UUID] = param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(e => ParseFailure(s"Failed to decode UUID", e.getMessage))
      .toValidatedNel

  object BookId extends QueryParamDecoderMatcher[UUID]("bookId")
  object AuthorId extends QueryParamDecoderMatcher[UUID]("authorId")
  object Title extends QueryParamDecoderMatcher[String]("title")
  object Genre extends QueryParamDecoderMatcher[String]("genre")

  def getRoutes[F[_] : Sync](service: BookService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / domain => for {
        response <- Ok("test")
      } yield response

      case GET -> Root / domain :? BookId(bookId) => for {
        bookOpt   <- service.findById(bookId)
        response  <- bookOpt.fold(NotFound(s"Can't find book with bookId=`$bookId`"))(Ok(_))
      } yield response
    }
  }

  def postRoutes[F[_] : Sync](service: BookService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case request @ POST -> Root / domain => for {
        book    <- request.as[NewBookDto]
        created <- service.create(book)

        response = created match {
          case Left(error)  => errorToHttpResponse(error)
          case Right(book)  => Ok(book)
        }
      } yield response
    }
  }

  def putRoutes[F[_] : Sync](service: BookService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case request @ PUT -> Root / domain :? BookId(bookId) => for {
        book    <- request.as[NewBookDto]
        updated <- service.update(bookId, book)

        response = updated match {
          case Left(error)  => errorToHttpResponse(error)
          case Right(book)  => Ok(book)
        }

      } yield response
    }
  }

  def deleteRoutes[F[_] : Sync](service: BookService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case request@DELETE -> Root / domain :? BookId(bookId) => for {
        deleted <- service.delete(bookId)

        response = deleted match {
          case Left(error) => errorToHttpResponse(error)
          case Right(bool) => Ok(bool)
        }

      } yield response
    }
  }

  def errorToHttpResponse(error: BookValidationError) = {
    error match {
      case e @ InvalidYear => BadRequest(e.message)
      case e @ BookNotFound => NotFound(e.message)
      case e @ AuthorNotFound => NotFound(e.message)

      case _ => InternalServerError()
    }
  }
}
