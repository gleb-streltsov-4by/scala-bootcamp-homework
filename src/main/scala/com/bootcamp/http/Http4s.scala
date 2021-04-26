package com.bootcamp.http

import cats.Functor
import cats.data.Kleisli
import cats.effect.{Blocker, ExitCode, IO, IOApp, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, Method, Request, Response, Uri}
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import io.circe.generic.JsonCodec
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext
import scala.util.Random
import com.bootcamp.http.GuessApi._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

object GuessApi {

  val apiVersion = "v1"

  val apiHost = "localhost"
  val apiPort = 9000
  val apiPath = "api"
  val apiUrl = s"http://$apiHost:$apiPort/$apiPath/$apiVersion"

  val apiInitMethod = "init"
  val apiGuessMethod = "guess"

  @JsonCodec final case class InitGeneratorRequest(min: Int, max: Int)

  @JsonCodec sealed trait GuessResult {
    def toString: String
  }

  final case object Match extends GuessResult { override def toString: String = "Match!" }
  final case object Greater extends GuessResult { override def toString: String = "Guess value is greater" }
  final case object Lower extends GuessResult { override def toString: String = "Guess value is lower" }
}

trait IOLogger {

  private val ioLogger: Logger = LoggerFactory.getLogger(this.getClass.toString)

  def ioInfo(message: String): IO[Unit] = IO(ioLogger.info(message))
}

trait EagerLogger {

  private val eagerLogger: Logger = LoggerFactory.getLogger(this.getClass.toString)

  def info(message: String): Unit = eagerLogger.info(message)
}

object GuessServer extends IOApp with IOLogger {

  override def run(args: List[String]): IO[ExitCode] = {
    RefGuessGenerator.from[IO].flatMap(generator =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = apiPort, host = apiHost)
        .withHttpApp(httpApp(generator))
        .serve
        .compile
        .drain
        .as(ExitCode.Success))
  }

  trait GuessGenerator[F[_]] {
    def init(min: Int, max: Int): F[Int]
    def guess(number: Int): F[GuessResult]
  }

  case class RefGuessGenerator[F[_] : Functor](private val ref: Ref[F, Int]) extends GuessGenerator[F] {
    override def init(min: Int, max: Int): F[Int] =
      ref.modify(_ => {
        val updated = min + Random.nextInt((max - min) + 1)
        (updated, updated) // 1 - set, 2 - return
      })

    override def guess(number: Int): F[GuessResult] =
      ref.get.map(current =>
        number - current match {
          case 0 => Match
          case x if x < 0 => Lower
          case x if x > 0 => Greater
        })
  }

  object RefGuessGenerator {
    def from[F[_] : Functor : Sync]: F[RefGuessGenerator[F]] =
      Ref.of[F, Int](0).map(RefGuessGenerator(_))
  }

  private def httpApp(generator: RefGuessGenerator[IO]): Kleisli[IO, Request[IO], Response[IO]] = {

    object GuessValue extends QueryParamDecoderMatcher[Int]("value")

    HttpRoutes.of[IO] {
      case request @ POST -> Root / apiPath / apiVersion / apiInitMethod => (for {
        validated   <- request.as[InitGeneratorRequest]
        guessValue  <- generator.init(validated.min, validated.max)

        _ <- ioInfo(s"Generated guess value: $guessValue")
      } yield Ok("Guess value is successfully generated")).flatten

      case GET -> Root /  apiPath / apiVersion / apiGuessMethod :? GuessValue(value) => (for {
        result <- generator.guess(value)
      } yield Ok(result)).flatten
    }
  }.orNotFound
}

object GuessClient extends IOApp with IOLogger with EagerLogger {

  private val min = 0
  private val max = 40

  private val uri = Uri.fromString(s"$apiUrl").fold(_ => uri"", u => u)

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global)
      .resource
      .parZip(Blocker[IO]).use { case (client, _) =>
        for {
          result <- client.expect[String](Method.POST(
            InitGeneratorRequest(min, max), uri / apiInitMethod))
          _ <- ioInfo(result)
          _ <- guessNumber(min, max)(client)
        } yield ()
      }.as(ExitCode.Success)
  }

  private def guessNumber(min: Int, max: Int)(
    implicit client: Client[IO]): IO[Boolean] = {
    val mid = (min + max) / 2
    client.expect[GuessResult]((uri / apiGuessMethod)
      .withQueryParam(key = "value", value = mid))
      .flatMap { result =>
        info(s"Request with guess value `$mid` has result: `${result.toString}`")

        result match {
          case Lower    => guessNumber(mid, max)
          case Greater  => guessNumber(min, mid)
          case Match    => IO(true)
          case _        => IO(false)
        }
      }
  }
}