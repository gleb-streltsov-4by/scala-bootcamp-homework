package com.bootcamp.basics

import com.bootcamp.basics.ClassesAndTraits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ClassesAndTraitsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "Parameters of square" should "return correct values" in {
    val square = Square(3, 3, 4)

    square.area shouldEqual 16
    square.minX shouldEqual 1
    square.maxX shouldEqual 5
    square.minY shouldEqual 1
    square.maxY shouldEqual 5
  }

  "Parameters of 2d triangle" should "return correct values" in {
    val triangle = Triangle2D(1, 1, -2, 4, -2, -2)

    triangle.area shouldEqual 9
    triangle.minX shouldEqual -2
    triangle.maxX shouldEqual 1
    triangle.minY shouldEqual -2
    triangle.maxY shouldEqual 4
  }

  "Parameters of sphere" should "return correct values" in {
    val sphere = Sphere(1, 1, -2, 2)

    sphere.surfaceArea shouldEqual 50.26548245743669
    sphere.volume shouldEqual 33.510321638291124

    sphere.minX shouldEqual -1
    sphere.maxX shouldEqual 3
    sphere.minY shouldEqual -1
    sphere.maxY shouldEqual 3
    sphere.minZ shouldEqual -4
    sphere.maxZ shouldEqual 0
  }

  "Parameters of cube" should "return correct values" in {
    val cube = Cube(1, 1, -2, 2)

    cube.surfaceArea shouldEqual 24
    cube.volume shouldEqual 8

    cube.minX shouldEqual 0
    cube.maxX shouldEqual 2
    cube.minY shouldEqual 0
    cube.maxY shouldEqual 2
    cube.minZ shouldEqual -3
    cube.maxZ shouldEqual -1
  }

  "Parameters of cuboid" should "return correct values" in {
    val cuboid = Cuboid(1, 1, -2, 2, 3, 4)

    cuboid.surfaceArea shouldEqual 52
    cuboid.volume shouldEqual 24

    cuboid.minX shouldEqual 0
    cuboid.maxX shouldEqual 2
    cuboid.minY shouldEqual -0.5
    cuboid.maxY shouldEqual 2.5
    cuboid.minZ shouldEqual -4
    cuboid.maxZ shouldEqual 0
  }

  "Parameters of 3d triangle" should "return correct values" in {
    val triangle = Triangle3D(
      1, 1, -2,
      2, 3, 4,
      4, 5, 1)

    triangle.area shouldEqual 23.473389188611005

    triangle.minX shouldEqual 1
    triangle.maxX shouldEqual 4
    triangle.minY shouldEqual 1
    triangle.maxY shouldEqual 5
    triangle.minZ shouldEqual -2
    triangle.maxZ shouldEqual 4
  }

}
