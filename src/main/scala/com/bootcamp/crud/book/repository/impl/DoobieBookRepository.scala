package com.bootcamp.crud.book.repository.impl

import com.bootcamp.crud.book.domain.book.BookWithAuthor
import com.bootcamp.crud.book.dto.book.NewBookDto
import com.bootcamp.crud.book.repository.BookRepository
import doobie.Transactor

import java.time.Year
import java.util.UUID

class DoobieBookRepository[F[_]](transactor: Transactor[F]) extends BookRepository[F]{

  override def findAll: F[List[BookWithAuthor]] = ???

  override def findById(bookId: UUID): F[Option[BookWithAuthor]] = ???

  override def findByAuthor(authorId: UUID): F[List[BookWithAuthor]] = ???

  override def findByTitle(title: String): F[List[BookWithAuthor]] = ???

  override def findByYear(year: Year): F[List[BookWithAuthor]] = ???

  override def findByGenre(genre: String): F[List[BookWithAuthor]] = ???

  override def create(newBook: NewBookDto): F[BookWithAuthor] = ???

  override def update(bookId: UUID, book: NewBookDto): F[BookWithAuthor] = ???

  override def delete(bookId: UUID): F[Boolean] = ???
}
