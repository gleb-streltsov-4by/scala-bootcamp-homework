package com.bootcamp.implicits

object QAndAExamples {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    // summoner
    def apply[F: Semigroup]: Semigroup[F] = implicitly

    // syntax so you can call `combine` after .
    implicit class SemigroupOps[F: Semigroup](x: F) {
      def combine(y: F): F = Semigroup[F].combine(x, y)
    }

    // 1.2. Implement Semigroup for Long, String

    // instances
    implicit val intSemigroup: Semigroup[Int] = (x, y) => x + y
    implicit val longSemigroup: Semigroup[Long] = (x, y) => x + y
    implicit val strSemigroup: Semigroup[String] = (x, y) => x + y
  }

  def main(args: Array[String]): Unit = {
    import Semigroup._

    print(1 combine 2)
    print("str".combine("abc"))
  }

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists

  import Semigroup._

  def combineAll[A: Semigroup](lst: List[A]): A = lst.reduce((a, b) => a.combine(b))
  def combineAll1[A: Semigroup](lst: List[A]): A = lst.reduce(_ combine _)

  val res: Boolean = combineAll(List(1, 2, 3)) == 6

  // Problem: handle empty lists

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists

  def combineAll2[A: Semigroup](list: List[A], startingElement: A): A =
    list.foldLeft(startingElement)(_ combine _)

   combineAll2(List(1, 2, 3), 0) == 6
   combineAll2(List(), 1) == 1

  // 2. Monoid // it is a type class / monad which provides empty value
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly

    // instances

    // 2.2. Implement Monoid for Long, String

    implicit val monoidLong: Monoid[Long] = new Monoid[Long] {
      override def empty: Long = 0
      override def combine(x: Long, y: Long): Long = x + x
    }

    implicit val monoidStr: Monoid[String] = new Monoid[String] {
      override def empty: String = ""
      override def combine(x: String, y: String): String = x + y
    }

    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0
      override def combine(x: Int, y: Int): Int = x + y
    }
  }

  // 2.3. Implement combineAll3(list: List[A]) for all lists

  def combineAll3[A: Monoid](lst: List[A]): A =
    lst.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  import Monoid._

  combineAll3(List(1, 2, 3)) == 6

  // 2.4. Implement Monoid for Option[A]
  implicit def monoidOpt[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None
    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xVal), Some(yVal)) => Some(Semigroup[A].combine(xVal, yVal))
      case (x, y)                   => x.orElse(y)
    }
  }

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def functionMonoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => Monoid[B].empty
    override def combine(x: A => B, y: A => B): A => B = a => Monoid[B].combine(x(a), y(a))
  }

  monoidOpt[Long].combine(Some(2), None) == Some(2)
  monoidOpt[Long].combine(None, Some(2)) == Some(2)

  combineAll3(List(Some(1), None, Some(3))) == Some(4)
//  combineAll3(List(None, None)) == None
//  combineAll3(List()) == None

   combineAll3(List((a: String) => a.length, (a: String) => a.toInt))         // === (a: String) => (a.length + a.toInt)
   combineAll3(List((a: String) => a.length, (a: String) => a.toInt)).apply("123") // === 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)

  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly[Semigroupal[F]]

    implicit class SemigroupalOps[F[_]: Semigroupal, A](fa: F[A]) {
      def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
    }

    // instances

    // 4.2. Implement Semigroupal for Option

    implicit val optSemi: Semigroupal[Option] = new Semigroupal[Option] {
      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
        case (Some(a), Some(b)) => Some(a, b)
        case _                  => None
      }
    }

//    implicit val optSemi: Semigroupal[Option] = new Semigroupal[Option] {
//      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
//        a <- fa
//        b <- fb
//      } yield (a, b)
//    }
  }

  import Semigroupal._

  println(Option(1).product(Option("2")))

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  implicit class mapNOps[F[_] : Functor : Semigroupal, A, B](x: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) => fa.product(fb).fmap {
        case (a, b) => f(a, b)
      }
    }
  }

  println(s"mapN1: ${(Option(1), Option("2")).mapN(_ + _.length)}")

  println(s"mapN2: ${(Option(1), Option(2)).mapN(_ + _)}")

//  (Option(1), Option(2)).mapN(_ + _) == Some(3)
//  (Option(1), None).mapN(_ + _)      == None

  // 4.4. Implement Semigroupal for Map

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)
    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = ???
    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = ???
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  // 6.3. Implement `traverse` for all Foldables instead of List
}

