package com.bootcamp.cats_effect

import cats.Applicative

import java.util.concurrent.Executors
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxApplicativeId

import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContext
import scala.io.{Source, StdIn}

object HashUtil {

  private val defaultSeed     = 100

  def javaHash(word: String, seed: Int = defaultSeed): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(word: String, seed: Int = defaultSeed): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % seed
  }

}

object IOUtil {

  sealed trait Error extends Throwable{
    def toString: String
  }

  object Error {
    final case object InvalidFilePath extends Error {
      override def toString: String = "File path is invalid ..."
    }
    final case object NoWordsInFile extends Error {
      override def toString: String = "File doesn't contain words ..."
    }
  }

  def printLine(s: String): IO[Unit] =
    for {
      _ <- IO(println(s))
      _ <- showThread
    } yield ()

  def readLine: IO[String] =
    for {
      line <- IO(StdIn.readLine())
      _ <- showThread
    } yield line

  def validateFilePath(filePath: String): IO[Unit] =
    for {
      path    <- IO(Paths.get(filePath))
      exists  <- IO(Files.isReadable(path))
      _       <- IO.raiseUnless(exists)(Error.InvalidFilePath)
      _       <- showThread
    } yield ()

  def validateSeed(seedStr: String): IO[Int] =
    for {
      seed  <- IO(seedStr.toInt)
      _     <- showThread
    } yield seed

  def validateFileContent(lines: List[String]): IO[Unit] =
    for {
      wordExists  <- IO(lines.find(_.split("[\\p{Punct}\\s]+").length > 0))
      _           <- IO.raiseUnless(wordExists.isEmpty)(Error.NoWordsInFile)
      _           <- showThread
    } yield ()

  def readFileLines(path: String): IO[List[String]] =
    for {
      source  <- IO(Source.fromFile(path))
      lines   <- IO(source.getLines().toList)
      _ <- showThread
    } yield lines

  def showThread: IO[Unit] =
    IO(
      println(s"${Console.RED}<<" +
        s"Executed on thread: `${Thread.currentThread().getName}``" +
        s">>${Console.RESET}")
    )
}

object EffectsHomework2 extends IOApp {

  import IOUtil._
  import HashUtil._

  final case class ReadResult(path: String, lines: List[String], seed: Int)

  trait Repository[F[_]] {
    def retrieve(path: String): F[Int]
    def store(path: String, hash: Int): F[Unit]
  }

  // very naive implementation for test
  class TestRepository[F[_] : Applicative] extends Repository[F] {
    private val storage = scala.collection.mutable.Map[String, Int]()

    override def retrieve(path: String): F[Int] = storage(path).pure[F]

    override def store(path: String, hash: Int): F[Unit] = {
      storage += (path -> hash)
      ().pure[F]
    }
  }

  object TestRepository {
    def apply(): TestRepository[Option] = new TestRepository[Option]
  }

  private def readData: IO[ReadResult] = Blocker[IO].use { blocker =>
    for {
      _ <- printLine("File path to calculate hash:")

      path  <- blocker.blockOn(readLine)
      _     <- validateFilePath(path)
      lines <- readFileLines(path)
      _     <- validateFileContent(lines)

      _ <- printLine("Seed to calculate hash:")

      seed      <- blocker.blockOn(readLine)
      validSeed <- validateSeed(seed)

    } yield ReadResult(path, lines, validSeed)
  }

  private def calculateSignatures(lines: List[String], seed: Int): IO[Int] = {
    val words = lines.flatMap(_.split("[\\p{Punct}\\s]+"))
    for {
      javaHash   <- IO(words.map(word => javaHash(word, seed)).min)
      knuthHash  <- IO(words.map(word => knuthHash(word, seed)).min)

      min = javaHash min knuthHash

    } yield min
  }

  private def calculateSignaturesPar(lines: List[String], seed: Int): IO[Int] = {
    val words = lines.flatMap(_.split("[\\p{Punct}\\s]+"))
    for {
      javaHashFiber   <- IO(words.map(word => javaHash(word, seed)).min).start
      knuthHashFiber  <- IO(words.map(word => knuthHash(word, seed)).min).start

      minJavaHash   <- javaHashFiber.join
      minKnuthHash  <- knuthHashFiber.join
    } yield minJavaHash min minKnuthHash
  }

  override def run(args: List[String]): IO[ExitCode] = {
    (for {
      result <- readData

      calculationExecutors        = Executors.newFixedThreadPool(2)
      calculationExecutionContext = ExecutionContext.fromExecutor(calculationExecutors)

      signature   <- contextShift.evalOn(calculationExecutionContext)(
        calculateSignaturesPar(result.lines, result.seed)
      ).guarantee(IO(calculationExecutors.shutdown()))

      repository = TestRepository()

      _ <- IO(repository.store(result.path, signature))
      _ <- printLine(s"Path: ${result.path} -> Hash: ${repository.retrieve(result.path)}")
    } yield ()) as ExitCode.Success
  }
}
