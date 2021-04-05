package com.bootcamp.cats_effect

import com.bootcamp.cats_effect.EffectsHomework1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EffectsHomework1Spec extends AnyFlatSpec with Matchers {

  "map" should "return correct values" in {
    val a = IO("abc")
    val c = a.map(_ => "bcd")
    c.unsafeRunSync() shouldEqual "bcd"
  }

  "flatMap" should "return correct values" in {
    val res = IO(4).flatMap(x => IO(x * 4))
    res.unsafeRunSync() shouldEqual 16
  }

  "option" should "return correct values" in {
    val input = IO("test").option
    input.unsafeRunSync() shouldEqual Some("test")
  }

  "void" should "return correct values" in {
    val input = IO("test").void
    input.unsafeRunSync() shouldEqual ()
  }
}
