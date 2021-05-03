package com.bootcamp.cats_effect

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (Long, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] =
      state.get.map { s => for {
          (_, value) <- s.get(key)
        } yield value
      }

    def put(key: K, value: V): F[Unit] = {
      for {
        currentTime <- Clock[F].realTime(MILLISECONDS)
        _           <- state.update(s =>
          s + (key -> (currentTime + expiresIn.toMillis, value)))
      } yield ()
    }

    def evictIfExpired: F[Unit] = for {
      now <- Clock[F].realTime(MILLISECONDS)
      _   <- state.update(_.filter {
        case (_, (ts, _)) => (now - ts) < expiresIn.toMillis
      })
    } yield ()

  }

  object Cache {

    def of[F[_] : Clock, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def checkExpirationEndlessly(cache: RefCache[F, K, V]): F[Unit] =
        for {
          _ <- T.sleep(checkOnExpirationsEvery)
          _ <- cache.evictIfExpired
          _ <- checkExpirationEndlessly(cache)
        } yield ()

      for {
        ref   <- Ref.of(Map.empty[K, (Long, V)])
        cache <- new RefCache(ref, expiresIn).pure[F]
        _     <- C.start(checkExpirationEndlessly(cache))
      } yield cache
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}