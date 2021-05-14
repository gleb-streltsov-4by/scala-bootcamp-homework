package com.bootcamp.crud.book.service.impl

import com.bootcamp.crud.book.domain.book._
import com.bootcamp.crud.book.dto.book.NewBookDto
import com.bootcamp.crud.book.repository.BookRepository
import com.bootcamp.crud.book.service.BookService
import com.bootcamp.crud.book.service.error.book._

import java.time.Year
import java.util.UUID

class BookServiceImpl[F[_]](bookRepository: BookRepository[F]) extends BookService[F] {

  override def findAll: F[List[BookWithAuthor]] = bookRepository.findAll
  override def findById(bookId: UUID): F[Option[BookWithAuthor]] = bookRepository.findById(bookId)
  override def findByAuthor(authorId: UUID): F[List[BookWithAuthor]] = bookRepository.findByAuthor(authorId)
  override def findByTitle(title: String): F[List[BookWithAuthor]] = bookRepository.findByTitle(title)
  override def findByYear(year: Year): F[List[BookWithAuthor]] = bookRepository.findByYear(year)
  override def findByGenre(genre: String): F[List[BookWithAuthor]] = bookRepository.findByGenre(genre)

  override def create(newBook: NewBookDto): F[Either[BookValidationError, BookWithAuthor]] = ???

  override def update(bookId: UUID, book: NewBookDto): F[Either[BookValidationError, BookWithAuthor]] = ???

  override def delete(bookId: UUID): F[Either[BookValidationError, Boolean]] = ???
}
