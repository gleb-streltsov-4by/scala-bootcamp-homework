package com.bootcamp.crud.book.service.impl

import com.bootcamp.crud.book.domain.book._
import com.bootcamp.crud.book.dto.book
import com.bootcamp.crud.book.service.BookService
import com.bootcamp.crud.book.service.error.book._

import java.time.Year
import java.util.UUID

class BookServiceImpl[F[_]] extends BookService[F] {

  override def findAll: F[List[BookWithAuthor]] = ???

  override def findById(bookId: UUID): F[Option[BookWithAuthor]] = ???

  override def findByAuthor(authorId: UUID): F[List[BookWithAuthor]] = ???

  override def findByTitle(title: String): F[List[BookWithAuthor]] = ???

  override def findByYear(year: Year): F[List[BookWithAuthor]] = ???

  override def findByGenre(genre: String): F[List[BookWithAuthor]] = ???

  override def create(newBook: book.NewBookDto): F[Either[BookValidationError, BookWithAuthor]] = ???

  override def update(bookId: UUID, book: BookWithAuthor): F[Either[BookValidationError, BookWithAuthor]] = ???

  override def delete(bookId: UUID): F[Either[BookValidationError, Boolean]] = ???
}
