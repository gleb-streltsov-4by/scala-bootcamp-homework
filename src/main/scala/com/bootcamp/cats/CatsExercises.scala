package com.bootcamp.cats

import cats.{Functor, Monad}

/*
    essential
      map      : F[A].map(A => B) = F[B]
      flatMap  : F[A].flatMap(A => F[B]) = F[B]
      sequence : List[F[A]].sequence = F[List[A]]

    convenient
      flatten  : F[F[A]].flatten = F[A]
      traverse : List[A].traverse(A => F[B]) = F[List[B]]
   */
object Exercises {

  trait Applicative[F[_]] extends Functor[F] {

    def map[A,B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    // implement methods using other methods
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(as => as)

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {

    def unit[A](a: => A): M[A]

    // implement methods using other methods
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(a => f(a)))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

    def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  }
}

object Monads {

  val optionMonad: Monad[Option] = new Monad[Option] {

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      fa.flatMap(a => f(a))
    }

    override def pure[A](x: A): Option[A] = {
      Option.apply(x)
    }

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  }

  def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {

    override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = {
      fa.flatMap(a => f(a))
    }

    override def pure[A](x: A): Either[T, A] = Right(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
  }

  def functionMonad[T]: Monad[T => *] = new Monad[T => *] {

    override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B = { a =>
      f(fa(a))(a)
    }

    override def pure[A](x: A): T => A = _ => x

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
  }
}

object FiveMonads {

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }


  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] = amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] = ma match {
        case Just(a)  => amb(a)
        case None     => None
      }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    // inspired by https://stackoverflow.com/a/6248296
    implicit def stateMonad[S] = new Monad[State[S, *]] {
      def unit[A](a: A): State[S, A] = State(s => (s, a))
      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] =
        State(x => {
          val (s, a) = ma.run(x)
          amb(a).run(s)
        })
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    implicit def readerMonad[R] = new Monad[Reader[R, *]] {
      def unit[A](a: A): Reader[R, A] = Reader(_ => a)
      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] =
        Reader(r => {
          val a = ma.run(r)
          amb(a).run(r)
        })
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[Writer[W, *]] {
      def unit[A](a: A): Writer[W, A] = Writer(m.mempty, a)
      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] = {
        val (wa, a) = ma.run
        val (wb, br) = amb(a).run
        val wr = m.mappend(wa)(wb)
        Writer((wr, br))
      }
    }
  }
}
