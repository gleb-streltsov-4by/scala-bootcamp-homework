package com.bootcamp.crud.book.service.error

object book {

  sealed trait BookValidationError extends Throwable {
    def message: String
  }

  object BookValidationError {
    final case object InvalidYear extends BookValidationError {
      override def message: String = "Book year can't be in future"
    }

    final case object AuthorNotFound extends BookValidationError {
      override def message: String = "Author not found"
    }

    final case object BookNotFound extends BookValidationError {
      override def message: String = "Book not found"
    }
  }
}
