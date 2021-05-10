package com.bootcamp.crud.book.service

import cats.effect.Sync
import com.bootcamp.crud.book.domain.book._
import com.bootcamp.crud.book.dto.book.NewBookDto
import com.bootcamp.crud.book.service.error.book.BookValidationError
import com.bootcamp.crud.book.service.impl.BookServiceImpl

import java.time.Year
import java.util.UUID

trait BookService[F[_]] {
  def findAll: F[List[BookWithAuthor]]
  def findById(bookId: UUID): F[Option[BookWithAuthor]]
  def findByAuthor(authorId: UUID): F[List[BookWithAuthor]]
  def findByTitle(title: String): F[List[BookWithAuthor]]
  def findByYear(year: Year): F[List[BookWithAuthor]]
  def findByGenre(genre: String): F[List[BookWithAuthor]]

  def create(newBook: NewBookDto): F[Either[BookValidationError, BookWithAuthor]]
  def update(bookId: UUID, book: NewBookDto): F[Either[BookValidationError, BookWithAuthor]]
  def delete(bookId: UUID): F[Either[BookValidationError, Boolean]]
}

object BookService {
  def impl[F[_]: Sync]: BookService[F] = new BookServiceImpl[F]
}