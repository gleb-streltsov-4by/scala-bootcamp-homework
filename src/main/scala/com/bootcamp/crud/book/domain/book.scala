package com.bootcamp.crud.book.domain

import java.time.Year
import java.util.UUID
import com.bootcamp.crud.book.domain.author._
import io.circe.generic.JsonCodec

object book {

  @JsonCodec final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: String)

  @JsonCodec final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year) {
    override def toString: String = s"$title ($year) by ${author.name}"
  }

}
