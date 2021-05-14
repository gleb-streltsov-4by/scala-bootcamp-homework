package com.bootcamp.crud.book.repository

import cats.effect.{Async, Blocker, ContextShift, Resource, Sync}
import com.bootcamp.crud.book.domain.book.BookWithAuthor
import com.bootcamp.crud.book.dto.book.NewBookDto
import com.bootcamp.crud.book.repository.impl.DoobieBookRepository
import doobie.hikari.HikariTransactor
import doobie.{ExecutionContexts, Transactor}

import java.time.Year
import java.util.UUID

trait BookRepository[F[_]] {
  def findAll: F[List[BookWithAuthor]]
  def findById(bookId: UUID): F[Option[BookWithAuthor]]
  def findByAuthor(authorId: UUID): F[List[BookWithAuthor]]
  def findByTitle(title: String): F[List[BookWithAuthor]]
  def findByYear(year: Year): F[List[BookWithAuthor]]
  def findByGenre(genre: String): F[List[BookWithAuthor]]

  def create(newBook: NewBookDto): F[BookWithAuthor]
  def update(bookId: UUID, book: NewBookDto): F[BookWithAuthor]
  def delete(bookId: UUID): F[Boolean]
}

object BookRepository {

  val dbDriverName = "org.h2.Driver"
  val dbUrl        = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"
  val dbUser       = ""
  val dbPwd        = ""

  private val poolSize = 10

  def impl[F[_]: ContextShift: Async]: BookRepository[F] = {
    initTransactor[F]
      .use(transactor =>
        new DoobieBookRepository[F](transactor))
  }

  private def initTransactor[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
    for {
      executionContext  <- ExecutionContexts.fixedThreadPool[F](poolSize)
      blocker           <- Blocker[F]

      transactor <- HikariTransactor.newHikariTransactor[F](

        driverClassName = dbDriverName,

        url   = dbUrl,
        user  = dbUser,
        pass  = dbPwd,

        connectEC = executionContext,
        blocker   = blocker
      )
    } yield transactor
}
