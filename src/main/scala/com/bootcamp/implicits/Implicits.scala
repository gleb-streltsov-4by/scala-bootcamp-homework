package com.bootcamp.implicits

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

// fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object Implicits {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    private val objectHeaderSizeScore: SizeScore = 12

    trait GetSizeScore[F] {
      def apply(value: F): SizeScore
    }

    object GetSizeScore {
      def apply[F: GetSizeScore]: GetSizeScore[F] = implicitly

      implicit class GetSizeScoreOps[F: GetSizeScore](inner: F) {
        def sizeScore: SizeScore = GetSizeScore[F].apply(inner)
      }

      /*
      Implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)
      */

      // default instances
      implicit def byteSizeScore: GetSizeScore[Byte]  = _ => 1
      implicit def charSizeScore: GetSizeScore[Char]  = _ => 2
      implicit def intSizeScore: GetSizeScore[Int]    = _ => 4
      implicit def longSizeScore: GetSizeScore[Long]  = _ => 8
      implicit def strSizeScore: GetSizeScore[String] = s => objectHeaderSizeScore + s.length * 2
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      // with this you can use .sizeScore syntax on keys and values
      import GetSizeScore._

      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        val delta = key.sizeScore + value.sizeScore
        val current = currentScoreSize
        if (delta < maxSizeScore) {
          evictIfRequired(delta, current)
          map(key) = value
        }
      }

      def get(key: K): Option[V] = map.get(key)

      private def currentScoreSize: Int = {
        map.foldLeft(0)((acc, pair) => acc + (pair match {
          case (k, v) => k.sizeScore + v.sizeScore
        }))
      }

      @tailrec
      private def evictIfRequired(delta: Int, current: Int): Unit = maxSizeScore - current match {
        case extra if extra < delta && map.nonEmpty =>
          val headOption = map.headOption
          val key = headOption.map { case (key, _) => key }.get
          val revealed = headOption.map { case (key, value) => key.sizeScore + value.sizeScore }.get

          map -= key
          evictIfRequired(delta, current - revealed)

        case _ => ()
      }
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      import GetSizeScore._

      def apply[F[_]](implicit instance: Iterate[F]): Iterate[F] = instance

      implicit class IterateOps[F[_]: Iterate, T: GetSizeScore](inner: F[T]) {
        def iterator: Iterator[T] = Iterate[F].iterator(inner)
      }

      // instances
      implicit def iterateSizeScore[F[_]: Iterate, T: GetSizeScore]: GetSizeScore[F[T]] =
        objectHeaderSizeScore + _.iterator.map(_.sizeScore).sum

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
    }

    // Provide Iterate2 instances for Map and PackedMultiMap!
    // if the code doesn't compile while you think it should - sometimes full rebuild helps!

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def keyIterator[T, S](f: F[T, S]): Iterator[T]
      def valueIterator[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      import GetSizeScore._

      def apply[F[_, _]](implicit instance: Iterate2[F]): Iterate2[F] = instance

      implicit class Iterate2Ops[F[_, _]: Iterate2, K: GetSizeScore, V: GetSizeScore](inner: F[K, V]) {
        def keyIterator: Iterator[K] = Iterate2[F].keyIterator(inner)
        def valueIterator: Iterator[V] = Iterate2[F].valueIterator(inner)
      }

      // instances
      implicit def iterate2SizeScore[F[_, _]: Iterate2, K: GetSizeScore, V: GetSizeScore]: GetSizeScore[F[K, V]] =
        x => objectHeaderSizeScore +
          x.keyIterator.map(_.sizeScore).sum +
          x.valueIterator.map(_.sizeScore).sum

      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def keyIterator[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator
        override def valueIterator[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val multiMapIterate2: Iterate2[PackedMultiMap] =
        new Iterate2[PackedMultiMap] {
          override def keyIterator[T, S](f: PackedMultiMap[T, S]): Iterator[T] =
            f.inner.map({ case (k, _) => k }).iterator

          override def valueIterator[T, S](f: PackedMultiMap[T, S]): Iterator[S] =
            f.inner.map({ case (_, v) => v }).iterator
        }
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import SuperVipCollections4s.GetSizeScore._
    import SuperVipCollections4s.Iterate._
    import SuperVipCollections4s.Iterate2._

    final case class Twit(
      id: Long,
      userId: Int,
      hashTags: Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes: List[FbiNote],
    )

    object Twit {
      implicit def twitSizeScore: GetSizeScore[Twit] = twit =>
        twit.id.sizeScore +
          twit.userId.sizeScore +
          twit.hashTags.sizeScore +
          twit.attributes.sizeScore +
          twit.fbiNotes.sizeScore
    }

    final case class FbiNote(
      month: String,
      favouriteChar: Char,
      watchedPewDiePieTimes: Long,
    )

    object FbiNote {
      implicit def fbiNote: GetSizeScore[FbiNote] = note => {
        note.month.sizeScore + note.favouriteChar.sizeScore + note.watchedPewDiePieTimes.sizeScore
      }
    }

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache() {

      private val mutableCache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = mutableCache.put(twit.id, twit)
      override def get(id: Long): Option[Twit] = mutableCache.get(id)
    }
  }
}
