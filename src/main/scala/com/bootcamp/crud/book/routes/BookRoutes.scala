package com.bootcamp.crud.book.routes

import cats.implicits._
import cats.data.Validated
import cats.effect.Sync
import com.bootcamp.crud.book.service.BookService
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder, Response}
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io._
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

  def routes[F[_] : Sync](service: BookService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def getRoutes: HttpRoutes[F] = {
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

    def postRoutes(): HttpRoutes[F] = {
      HttpRoutes.of[F] {
        case request @ POST -> Root / domain =>
          val response = for {
            book    <- request.as[NewBookDto]
            created <- service.create(book)

            response = created match {
              case Left(error)  => errorToHttpResponse(error)
              case Right(book)  => Ok(book)
            }
          } yield response

          response.flatten
      }
    }

    def putRoutes(): HttpRoutes[F] = {
      HttpRoutes.of[F] {
        case request @ PUT -> Root / domain :? BookId(bookId) =>
          val response = for {
            book    <- request.as[NewBookDto]
            updated <- service.update(bookId, book)

            response = updated match {
              case Left(error)  => errorToHttpResponse(error)
              case Right(book)  => Ok(book)
            }
          } yield response

          response.flatten
      }
    }

    def deleteRoutes(): HttpRoutes[F] = {
      HttpRoutes.of[F] {
        case DELETE -> Root / domain :? BookId(bookId) =>
          val response = for {
            deleted <- service.delete(bookId)

            response = deleted match {
              case Left(error) => errorToHttpResponse(error)
              case Right(bool) => Ok(bool)
            }
          } yield response

          response.flatten
      }
    }

    def errorToHttpResponse(error: BookValidationError): F[Response[F]] = {
      error match {
        case e @ InvalidYear => BadRequest(e.message)
        case e @ BookNotFound => NotFound(e.message)
        case e @ AuthorNotFound => NotFound(e.message)

        case _ => InternalServerError()
      }
    }

    getRoutes <+> putRoutes() <+> postRoutes() <+> deleteRoutes()
  }
}
