package com.bootcamp.crud.book.domain

import io.circe.generic.JsonCodec

import java.time.LocalDate
import java.util.UUID

object author {

  @JsonCodec final case class Author(id: UUID, name: String, birthday: LocalDate)

}
