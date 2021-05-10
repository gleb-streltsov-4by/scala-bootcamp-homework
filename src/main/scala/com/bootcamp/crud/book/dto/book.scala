package com.bootcamp.crud.book.dto

import io.circe.generic.JsonCodec

import java.time.Year
import java.util.UUID

object book {

  @JsonCodec final case class NewBookDto(authorId: UUID, title: String, year: Year, genre: String)
}
