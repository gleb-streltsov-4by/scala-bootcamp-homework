package com.bootcamp.crud.book

import cats.effect.{ExitCode, IO, IOApp}
import com.bootcamp.crud.book.servers.BookServer

object Main extends IOApp {
  def run(args: List[String]):IO[ExitCode] =
    BookServer.stream[IO].compile.drain.as(ExitCode.Success)
}
