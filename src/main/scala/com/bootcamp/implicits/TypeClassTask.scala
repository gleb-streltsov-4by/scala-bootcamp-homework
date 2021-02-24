package com.bootcamp.implicits

import com.bootcamp.implicits.TypeClassTask.HashCode._

object TypeClassTask {

  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance

    implicit class HashCodeOps[A](x: A) {
      def hash(implicit instance: HashCode[A]): Int = instance.hash(x)
    }

    // Instances
    implicit val stringHashCode: HashCode[String] = _.hashCode
    implicit val doubleHashCode: HashCode[Double] = _.hashCode
    implicit val intHashCode: HashCode[Int]       = _.hashCode
  }

  def main(args: Array[String]): Unit = {
    println(s"Hashcode String: ${"test hash code".hash}")
    println(s"Hashcode Double: ${23.34d.hash}")
    println(s"Hashcode Integer: ${2021.hash}")
  }
}

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (a, b) => a.amount.compare(b.amount)

  def main(args: Array[String]): Unit = {
    val lst = List(Money(5), Money(1.23), Money(3.2), Money(7.799), Money(0))
    println(lst.sorted)
  }
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  object Show {
    def apply[F](implicit instance: Show[F]): Show[F] = instance

    implicit class ShowOps[A](x: A) {
      def show(implicit instance: Show[A]): String = instance.show(x)
    }

    // Instances
    implicit val userShow: Show[User] = user => s"${user.id} : ${user.name}"
  }

  def main(args: Array[String]): Unit = {
    import com.bootcamp.implicits.Task2.Show._

    println(User("1", "Oleg").show)
  }
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  sealed abstract case class User private (id: String, name: String)

  object User {
    def from(id: String, name: String): Either[Error, User] = {
      for {
        id <- Either.cond(id.nonEmpty, id, "user can't have empty id ")
        name <- Either.cond(name.nonEmpty, name, "user can't have empty name")
      } yield new User(id, name) {}
    }
  }

  object Parse {
    def apply[F](implicit instance: Parse[F]): Parse[F] = instance

    implicit class ParseOps(entity: String) {
      def parse[A](implicit instance: Parse[A]): Either[Error, A] = instance.parse(entity)
    }

    // Instances
    implicit val userParser: Parse[User] = { str =>
      str.trim.split("\\s+").toList match {
        case id :: name :: Nil  => User.from(id, name)
        case _                  => Left("Invalid string format for creation of user")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    import com.bootcamp.implicits.Task3.Parse._

    println("1 Oleg")
    println("lalala".parse[User])
    println("la    ".parse[User])
    println(" dfsdf la    ".parse[User])
  }
}

object Task4 {
  trait Equals[T] {
    def equals(a: T, b: T): Boolean
  }

  object Equals {
    def apply[F](implicit instance: Equals[F]): Equals[F] = instance

    implicit class EqualsOps[A](a: A) {
      def eqs(b: A)(implicit instance: Equals[A]): Boolean = instance.equals(a, b)
      def ===(b: A)(implicit instance: Equals[A]): Boolean = instance.equals(a, b)
    }

    // Instances
    implicit val intEquals: Equals[Int] = (a, b) => a == b
    implicit val stringEquals: Equals[String] = (a, b) => a.equalsIgnoreCase(b)
  }

  def main(args: Array[String]): Unit = {
    import com.bootcamp.implicits.Task4.Equals._

    println(1 === 3)
    println(1 === 1)
    println(1 eqs 1)
    println("a" eqs "A")
    println("a" === "b")
    //println(1d === 2d)
  }
}

object AdvancedHomework {
  trait CustomFlatMap[F[_]] {
    def customFlatMap[A, B](f: A => F[B])(x: F[A]): F[B]
  }

  object CustomFlatMap {
    def apply[F[_]: CustomFlatMap]: CustomFlatMap[F] = implicitly

    implicit class FlatMapOps[F[_]: CustomFlatMap, A](x: F[A]) {
      def customFlatMap[B](f: A => F[B]): F[B] = CustomFlatMap[F].customFlatMap(f)(x)
    }

    implicit val optFlatMap: CustomFlatMap[Option] = new CustomFlatMap[Option] {
      def customFlatMap[A, B](f: A => Option[B])(x: Option[A]): Option[B] = x.flatMap(f)
    }
  }

  def main(args: Array[String]): Unit = {
    import com.bootcamp.implicits.AdvancedHomework.CustomFlatMap._

    Option(1).customFlatMap(v => {
      println(v)
      Option(v)
    })

    val testNone: Option[Int] = None
    testNone.customFlatMap(v => {
      println(v)
      Option(v)
    })
  }

}